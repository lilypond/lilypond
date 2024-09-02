#!@TARGET_PYTHON@
# -*- coding: utf-8 -*-
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2005--2023  Han-Wen Nienhuys <hanwen@xs4all.nl>,
#                           Jan Nieuwenhuizen <janneke@gnu.org>,
#                           Reinhold Kainhofer <reinhold@kainhofer.com>,
#                           Patrick L. Schmidt <pls@philomelos.net>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


# `musicxml2ly` converts MusicXML input files (currently supporting a subset
# of version 2.0) to output files containing LilyPond source code.  This is
# done in three phases.
#
# 1.  Use Python's `xml.dom.minidom` API to parse the MusicXML file and
#     convert all data structures of the XML tree to a class hierarchy based
#     on the `Xml_node` class, adding class variables and methods as needed.
#     These classes can be found in file `musicxml.py` (providing module
#     `musicxml`).
#
# 2.  Extract the global paper and score information from the `Xml_node`
#     tree (see function `convert` in `musicxml2ly.py`).
#
# 3a. Walk over the `Xml_node` tree and extract all music (voices, staves,
#     parts, etc.), reordering and mapping the tree data as necessary (see
#     step 3a).  The central function is `musicxml_voice_to_lily_voice`,
#     filling up instances of the `LilyPondVoiceBuilder` class.  This
#     happens in file `musicxml2ly.py`; it is the most complicated part of
#     the conversion since MusicXML is a representation of *how to draw* a
#     score, making it non-trivial to convert its elements to the semantic
#     input (in most cases) needed by LilyPond.
#
# 3b. Function `musicxml_voice_to_lily_voice` and all functions eventually
#     called by it map `Xml_node` tree elements to corresponding output
#     classes that contain information how to print them.  The source code
#     formatting is done by class `Output_printer`, and output class names
#     have either the same name as `Xml_node` tree classes or the word
#     `Event` appended to it.  These classes can be found in file
#     `musicexp.py` (providing module `musicexp`), together with classes
#     `Paper` and `Layout` to print global paper and score information.


from collections import OrderedDict
from fractions import Fraction
from math import gcd
from functools import reduce
import gettext
import io
import optparse
import os
import re
import sys
import tempfile
import warnings
import zipfile

"""
@relocate-preamble@
"""

import musicexp
import musicxml
import musicxml2ly_globvars as globvars
import musicxml2ly_conversion as conversion
import musicxml2ly_definitions as definitions
import utilities

# Load translation and install _() into Python's builtins namespace.
gettext.install('lilypond', '@localedir@')

import lilylib as ly

lilypond_version = "@TOPLEVEL_VERSION@"

# Store command-line options in a global variable, so we can access them
# everywhere.
options = None


class Conversion_Settings:
    def __init__(self):
        self.ignore_beaming = False
        self.convert_stem_directions = False
        self.convert_rest_positions = True


conversion_settings = Conversion_Settings()

needed_additional_definitions = []

# This dictionary holds macros created dynamically by `musicxml2ly`, and
# which might access elements from `definitions.additional_definitions`.
additional_macros = {}


def round_to_two_digits(val):
    return round(val * 100) / 100


def extract_paper_information(score_partwise):
    defaults = score_partwise.get_maybe_exist_named_child('defaults')
    if not defaults:
        return None

    one_tenth_in_mm = -1

    scaling = defaults.get_maybe_exist_named_child('scaling')
    if scaling:
        millimeters_elem = scaling.get_named_child('millimeters')
        millimeters = float(millimeters_elem.get_text())

        # A normal five-line staff measures 40 tenths vertically.
        tenths_elem = scaling.get_maybe_exist_named_child('tenths')
        tenths = float(tenths_elem.get_text())

        one_tenth_in_mm = millimeters / tenths

        # In LilyPond, 72.27 points equal one inch.
        staff_size_in_mm = 40 * one_tenth_in_mm
        staff_size_in_pt = staff_size_in_mm * 72.27 / 25.4
        if 1 < staff_size_in_pt < 100:
            globvars.paper.global_staff_size = staff_size_in_pt
        else:
            size = 'small' if staff_size_in_pt <= 1 else 'large'
            ly.warning(_('requested global staff size (%.2fmm=%.2fpt) '
                         'is too %s, using %spt instead')
                       % (staff_size_in_mm, staff_size_in_pt, size,
                          globvars.paper.default_global_staff_size))

    # We need a valid tenth value for the rest of this function.
    if one_tenth_in_mm <= 0:
        return None

    def tenths_to_cm(txt):
        return round_to_two_digits(float(txt) * one_tenth_in_mm / 10)

    def set_paper_variable(varname, parent, element_name):
        el = parent.get_maybe_exist_named_child(element_name)
        if el:
            setattr(globvars.paper, varname, tenths_to_cm(el.get_text()))

    pagelayout = defaults.get_maybe_exist_named_child('page-layout')
    if pagelayout:
        # TODO: How can one have different margins for even and odd pages???
        set_paper_variable("page_height", pagelayout, 'page-height')
        set_paper_variable("page_width", pagelayout, 'page-width')

        if conversion_settings.convert_page_margins:
            pmargins = pagelayout.get_named_children('page-margins')
            for pm in pmargins:
                set_paper_variable("left_margin", pm, 'left-margin')
                set_paper_variable("right_margin", pm, 'right-margin')
                set_paper_variable("bottom_margin", pm, 'bottom-margin')
                set_paper_variable("top_margin", pm, 'top-margin')

    systemlayout = defaults.get_maybe_exist_named_child('system-layout')
    if systemlayout:
        sl = systemlayout.get_maybe_exist_named_child('system-margins')
        if sl:
            set_paper_variable("system_left_margin", sl, 'left-margin')
            set_paper_variable("system_right_margin", sl, 'right-margin')
        set_paper_variable("system_distance", systemlayout, 'system-distance')
        set_paper_variable("top_system_distance",
                           systemlayout, 'top-system-distance')

    stafflayout = defaults.get_named_children('staff-layout')
    for sl in stafflayout:
        nr = getattr(sl, 'number', 1)
        dist = sl.get_named_child('staff-distance')
        # TODO: the staff distance needs to be set in the Staff context!!!

    # TODO: Finish appearance?, music-font?, word-font?, lyric-font*,
    #       lyric-language*
    appearance = defaults.get_named_child('appearance')
    if appearance:
        lws = appearance.get_named_children('line-width')
        for lw in lws:
            # Possible types are: beam, bracket, dashes,
            #    enclosure, ending, extend, heavy barline, leger,
            #    light barline, octave shift, pedal, slur middle, slur tip,
            #    staff, stem, tie middle, tie tip, tuplet bracket, and wedge
            tp = lw.type
            w = tenths_to_cm(lw.get_text())
            # TODO: Do something with these values!
        nss = appearance.get_named_children('note-size')
        for ns in nss:
            # Possible types: `cue`, `grace`, `grace-cue`, `large`.
            tp = ns.type
            sz = tenths_to_cm(ns.get_text())
            # TODO: Do something with these values!
        # <other-appearance> elements have no specified meaning

    rawmusicfont = defaults.get_named_child('music-font')
    if rawmusicfont:
        # TODO: Convert the font
        pass
    rawwordfont = defaults.get_named_child('word-font')
    if rawwordfont:
        # TODO: Convert the font
        pass
    rawlyricsfonts = defaults.get_named_children('lyric-font')
    for lyricsfont in rawlyricsfonts:
        # TODO: Convert the font
        pass

    return globvars.paper


credit_dict = {
    None: None,
    '': None,
    'page number': None,  # TODO: what is it used for ?
    'title': 'title',
    'subtitle': 'subtitle',
    'composer': 'composer',
    'arranger': 'arranger',
    'lyricist': 'poet',
    'rights': 'copyright'
}


# Score information is contained in the <work>, <identification> or
# <movement-title> tags.  Extract those into a hash, indexed by proper
# lilypond header attributes.
def extract_score_information(tree):
    header = musicexp.Header()

    def set_if_exists(field, value):
        if value:
            if field == 'texidoc':
                # Don't surround the string with doublequotes yet so that it
                # gets split into words.  Doublequotes are added later in
                # function `dump_texidoc`.
                header.set_field(field, value.replace('"', r'\"'))
            else:
                header.set_field(field,
                                 utilities.escape_ly_output_string(value))

    movement_title = tree.get_maybe_exist_named_child('movement-title')
    movement_number = tree.get_maybe_exist_named_child('movement-number')
    if movement_title:
        set_if_exists('title', movement_title.get_text())
    if movement_number:
        set_if_exists('movementnumber', movement_number.get_text())
        # The movement number should be visible in the score.
        # set_if_exists('piece', movement_number.get_text())

    work = tree.get_maybe_exist_named_child('work')
    if work:
        work_number = work.get_work_number()
        work_title = work.get_work_title()
        # Overwrite the title from movement-title with work->title
        set_if_exists('title', work.get_work_title())
        set_if_exists('opus', work.get_work_number())
        # Use movement-title as subtitle
        if movement_title:
            set_if_exists('subtitle', movement_title.get_text())

    # TODO: Translation of opus element.  Not to be confused with opus in
    #       LilyPond.  MusicXML opus is a document element for opus DTD.
    identifications = tree.get_named_children('identification')
    for ids in identifications:
        set_if_exists('copyright', ids.get_rights())
        set_if_exists('composer', ids.get_composer())
        set_if_exists('arranger', ids.get_arranger())
        set_if_exists('editor', ids.get_editor())
        set_if_exists('poet', ids.get_poet())

        set_if_exists('encodingsoftware', ids.get_encoding_software())
        set_if_exists('encodingdate', ids.get_encoding_date())
        set_if_exists('encoder', ids.get_encoding_person())
        set_if_exists('encodingdescription', ids.get_encoding_description())
        set_if_exists('source', ids.get_source())

        # <miscellaneous><miscellaneous-field name="description"> ... becomes
        # \header { texidoc = ...
        set_if_exists('texidoc', ids.get_file_description())

        # Finally, apply the required compatibility modes
        # Some applications created wrong MusicXML files, so we need to
        # apply some compatibility mode, e.g. ignoring some features/tags
        # in those files
        software = ids.get_encoding_software_list()

        # Case 1: "Sibelius 5.1" with the "Dolet 3.4 for Sibelius" plugin
        #         is missing all beam ends => ignore all beaming information
        ignore_beaming_software = {
            "Dolet 4 for Sibelius, Beta 2": "Dolet 4 for Sibelius, Beta 2",
            "Dolet 3.5 for Sibelius": "Dolet 3.5 for Sibelius",
            "Dolet 3.4 for Sibelius": "Dolet 3.4 for Sibelius",
            "Dolet 3.3 for Sibelius": "Dolet 3.3 for Sibelius",
            "Dolet 3.2 for Sibelius": "Dolet 3.2 for Sibelius",
            "Dolet 3.1 for Sibelius": "Dolet 3.1 for Sibelius",
            "Dolet for Sibelius 1.3": "Dolet for Sibelius 1.3",
            "Noteworthy Composer": "Noteworthy Composer's nwc2xm[",
        }
        for s in software:
            app_description = ignore_beaming_software.get(s, False)
            if app_description:
                conversion_settings.ignore_beaming = True
                ly.warning(_("Encountered file created by %s, containing "
                             "wrong beaming information. All beaming "
                             "information in the MusicXML file will be "
                             "ignored") % app_description)

    credits = tree.get_named_children('credit')
    has_composer = False
    for cred in credits:
        type = credit_dict.get(cred.get_type())
        if type is None:
            type = credit_dict.get(cred.find_type(credits))
        if type == 'composer':
            if has_composer:
                type = 'poet'
            else:
                has_composer = True
            set_if_exists(type, cred.get_text())
        elif type == 'title':
            if not work and not movement_title:
                set_if_exists('title', cred.get_text())
            # elif (not(movement_title)):  # bullshit!
            #    # bullshit! otherwise both title and subtitle show the work
            #    # title.
            #    set_if_exists('subtitle', cred.get_text())
        elif type is None:
            pass
        else:
            set_if_exists(type, cred.get_text())

    # TODO: Check for other unsupported features
    return header


class PartGroupInfo(musicexp.Base):
    def __init__(self):
        self.start = {}
        self.end = {}

    def is_empty(self):
        return len(self.start) + len(self.end) == 0

    def add_start(self, g):
        self.start[getattr(g, 'number', "1")] = g

    def add_end(self, g):
        self.end[getattr(g, 'number', "1")] = g

    def print_ly(self, printer):
        ly.warning(_("Unprocessed PartGroupInfo %s encountered") % self)

    def ly_expression(self):
        ly.warning(_("Unprocessed PartGroupInfo %s encountered") % self)
        return ''


def staff_attributes_to_string_tunings(mxl_attr):
    details = mxl_attr.get_maybe_exist_named_child('staff-details')
    if not details:
        return []
    lines = 6
    staff_lines = details.get_maybe_exist_named_child('staff-lines')
    if staff_lines:
        lines = int(staff_lines.get_text())
    tunings = [musicexp.Pitch()] * lines
    staff_tunings = details.get_named_children('staff-tuning')
    for i in staff_tunings:
        p = musicexp.Pitch()
        line = 0
        try:
            line = int(i.line) - 1
        except ValueError:
            pass
        tunings[line] = p

        step = i.get_named_child('tuning-step')
        step = step.get_text().strip()
        p.step = conversion.musicxml_step_to_lily(step)

        octave = i.get_named_child('tuning-octave')
        octave = octave.get_text().strip()
        p.octave = int(octave) - 4

        alter = i.get_named_child('tuning-alter')
        if alter:
            p.alteration = int(alter.get_text().strip())
    # lilypond seems to use the opposite ordering than MusicXML...
    tunings.reverse()
    return tunings


def staff_attributes_to_lily_staff(mxl_attr):
    if not mxl_attr:
        return musicexp.Staff()

    (staff_id, attributes) = list(mxl_attr.items())[0]

    # We only distinguish between `TabStaff` and `Staff`.  Changing from a
    # tablature to a normal staff (and vice versa) in the middle of a piece
    # is allowed by MusicXML, but let's simplify the code by not supporting
    # such peculiarities.
    #
    # Note that we can't use `RhythmicStaff` or `DrumStaff`.  For the
    # former, there is no guarantee that notes are always placed on its
    # single staff line (which `RhythmicStaff` enforces).  For the latter,
    # MusicXML doesn't provide a set of pre-defined 'drums' that could be
    # used in `\drummode`, and `DrumStaff` also squeezes normal pitches to
    # be positioned on its middle staff line.
    clef_sign = None
    clef = attributes.get_maybe_exist_named_child('clef')
    if clef:
        sign = clef.get_maybe_exist_named_child('sign')
        if sign:
            clef_sign = sign.get_text()

    if clef_sign == 'TAB':
        staff = musicexp.TabStaff()
        staff.string_tunings = staff_attributes_to_string_tunings(attributes)
    else:
        staff = musicexp.Staff()

    return staff


def extract_instrument_sound(score_part):
    score_instrument = score_part.get_maybe_exist_named_child(
        'score-instrument')
    if not score_instrument:
        return None
    sound = score_instrument.get_maybe_exist_named_child('instrument-sound')
    if sound:
        return utilities.musicxml_sound_to_lilypond_midi_instrument(
            sound.get_text())


def extract_score_structure(part_list, staffinfo):
    score = musicexp.Score()
    structure = musicexp.StaffGroup(None)
    score.set_contents(structure)

    if not part_list:
        return structure

    def read_score_part(el):
        if not isinstance(el, musicxml.Score_part):
            return
        # Depending on the attributes of the first measure we create
        # different types of staves (`Staff` or `TabStaff`).
        staff = staff_attributes_to_lily_staff(staffinfo.get(el.id, None))
        if not staff:
            return None
        staff.id = el.id
        partname = el.get_maybe_exist_named_child('part-name')
        # Finale gives unnamed parts the name "MusicXML Part" automatically!
        if partname and partname.get_text() != "MusicXML Part":
            staff.instrument_name = partname.get_text()
        # part-name-display overrides part-name!
        partname = el.get_maybe_exist_named_child("part-name-display")
        if partname:
            staff.instrument_name = extract_display_text(partname)
        if options.midi:
            staff.sound = extract_instrument_sound(el)
        if staff.instrument_name:
            globvars.paper.indent = max(globvars.paper.indent,
                                       len(staff.instrument_name))
            globvars.paper.instrument_names.append(staff.instrument_name)
        partdisplay = el.get_maybe_exist_named_child('part-abbreviation')
        if partdisplay:
            staff.short_instrument_name = partdisplay.get_text()
        # part-abbreviation-display overrides part-abbreviation!
        partdisplay = el.get_maybe_exist_named_child(
            "part-abbreviation-display")
        if partdisplay:
            staff.short_instrument_name = extract_display_text(partdisplay)
        # TODO: Read in the MIDI device / instrument
        if staff.short_instrument_name:
            globvars.paper.short_indent = max(globvars.paper.short_indent,
                                             len(staff.short_instrument_name))

        return staff

    def read_score_group(el):
        if not isinstance(el, musicxml.Part_group):
            return
        group = musicexp.StaffGroup()
        group_id = getattr(el, 'number', None)
        if group_id is not None:
            group.id = group_id
        # PERF: Avoid the following multiple searches.
        if el.get_maybe_exist_named_child('group-name'):
            group.instrument_name = el.get_maybe_exist_named_child(
                'group-name').get_text()
        if el.get_maybe_exist_named_child('group-abbreviation'):
            group.short_instrument_name = el.get_maybe_exist_named_child(
                'group-abbreviation').get_text()
        if el.get_maybe_exist_named_child('group-symbol'):
            group.symbol = el.get_maybe_exist_named_child(
                'group-symbol').get_text()
        if el.get_maybe_exist_named_child('group-barline'):
            group.spanbar = el.get_maybe_exist_named_child(
                'group-barline').get_text()
        return group

    parts_groups = part_list.get_all_children()

    # the start/end group tags are not necessarily ordered correctly and groups
    # might even overlap, so we can't go through the children sequentially!

    # 1) Replace all Score_part objects by their corresponding Staff objects,
    #    also collect all group start/stop points into one PartGroupInfo object
    staves = []
    group_info = PartGroupInfo()
    for el in parts_groups:
        if isinstance(el, musicxml.Score_part):
            if not group_info.is_empty():
                staves.append(group_info)
                group_info = PartGroupInfo()
            staff = read_score_part(el)
            if staff:
                staves.append(staff)
        elif isinstance(el, musicxml.Part_group):
            if el.type == "start":
                group_info.add_start(el)
            elif el.type == "stop":
                group_info.add_end(el)
    if not group_info.is_empty():
        staves.append(group_info)

    # 2) Now, detect the groups:
    group_starts = []
    pos = 0
    while pos < len(staves):
        el = staves[pos]
        if isinstance(el, PartGroupInfo):
            prev_start = 0
            if len(group_starts) > 0:
                prev_start = group_starts[-1]
            elif len(el.end) > 0:  # no group to end here
                el.end = {}
            if len(el.end) > 0:  # closes an existing group
                ends = list(el.end.keys())
                prev_started = list(staves[prev_start].start.keys())
                grpid = None
                intersection = [x for x in prev_started if x in ends]
                if len(intersection) > 0:
                    grpid = intersection[0]
                else:
                    # Close the last started group
                    grpid = list(staves[prev_start].start.keys())[0]
                    # Find the corresponding closing tag and remove it!
                    j = pos + 1
                    foundclosing = False
                    while j < len(staves) and not foundclosing:
                        if (isinstance(staves[j], PartGroupInfo)
                                and grpid in staves[j].end):
                            foundclosing = True
                            del staves[j].end[grpid]
                            if staves[j].is_empty():
                                del staves[j]
                        j += 1
                grpobj = staves[prev_start].start[grpid]
                group = read_score_group(grpobj)
                # remove the id from both the start and end
                if grpid in el.end:
                    del el.end[grpid]
                del staves[prev_start].start[grpid]
                if el.is_empty():
                    del staves[pos]
                # replace the staves with the whole group
                for j in staves[(prev_start + 1):pos]:
                    group.append_staff(j)
                del staves[(prev_start + 1):pos]
                staves.insert(prev_start + 1, group)
                # reset pos so that we continue at the correct position
                pos = prev_start
                # remove an empty start group
                if staves[prev_start].is_empty():
                    del staves[prev_start]
                    group_starts.remove(prev_start)
                    pos -= 1
            elif len(el.start) > 0:  # starts new part groups
                group_starts.append(pos)
        pos += 1

    for i in staves:
        structure.append_staff(i)
    return score


def musicxml_partial_to_lily(partial_len):
    if partial_len > 0:
        p = musicexp.Partial()
        p.partial = musicexp.Duration.from_fraction(partial_len)
        return p
    else:
        return None


# Detect repeats and alternative endings in the chord event list(music_list)
# and convert them to the corresponding musicexp objects, containing nested
# music
def group_repeats(music_list):
    repeat_replaced = True
    music_start = 0
    i = 0
    # Walk through the list of expressions, looking for repeat structures
    # (repeat start/end, corresponding endings).  If we find one, try to
    # find the last event of the repeat, replace the whole structure, and
    # start over again.  For nested repeats, as soon as we encounter another
    # starting repeat bar, treat that one first, and start over for the
    # outer repeat.
    while repeat_replaced and i < 100:
        i += 1
        repeat_start = -1  # position of repeat start / end
        repeat_end = -1  # position of repeat start / end
        repeat_times = 0
        ending_start = -1  # position of current ending start
        endings = []  # list of already finished endings
        pos = 0
        last = len(music_list) - 1
        repeat_replaced = False
        final_marker = 0
        while pos < len(music_list) and not repeat_replaced:
            e = music_list[pos]
            repeat_finished = False
            if isinstance(e, conversion.RepeatMarker):
                if not repeat_times and e.times:
                    repeat_times = e.times
                if e.direction == -1:
                    if repeat_end >= 0:
                        repeat_finished = True
                    else:
                        repeat_start = pos
                        repeat_end = -1
                        ending_start = -1
                        endings = []
                elif e.direction == 1:
                    if repeat_start < 0:
                        repeat_start = 0
                    if repeat_end < 0:
                        repeat_end = pos
                    final_marker = pos
            elif isinstance(e, conversion.EndingMarker):
                if e.direction == -1:
                    if repeat_start < 0:
                        repeat_start = 0
                    if repeat_end < 0:
                        repeat_end = pos
                    ending_start = pos
                elif e.direction == 1:
                    if ending_start < 0:
                        ending_start = 0
                    endings.append([ending_start, pos])
                    ending_start = -1
                    final_marker = pos
            elif (not isinstance(e, musicexp.BarLine)
                  or (isinstance(e, musicexp.BarLine)
                      and e.type is not None)):
                # As soon as we encounter an element when repeat start and
                # end is set, and we are not inside an alternative ending
                # (also ignoring `BarLine` nodes that only set the bar
                # number), this whole repeat structure is finished, and we
                # can replace it.
                if repeat_start >= 0 and repeat_end > 0 and ending_start < 0:
                    repeat_finished = True

            # Finish off all repeats without an explicit ending bar (e.g.,
            # when we convert only one page of a multi-page score with
            # repeats).
            if pos == last and repeat_start >= 0:
                repeat_finished = True
                final_marker = pos
                if repeat_end < 0:
                    repeat_end = pos
                if ending_start >= 0:
                    endings.append([ending_start, pos])
                    ending_start = -1

            if repeat_finished:
                # We found the whole structure replace it!
                r = musicexp.RepeatedMusic()
                if repeat_times <= 0:
                    repeat_times = 2
                r.repeat_count = repeat_times
                # don't erase the first element for "implicit" repeats(i.e. no
                # starting repeat bars at the very beginning)
                start = repeat_start + 1
                if repeat_start == music_start:
                    start = music_start
                r.set_music(music_list[start:repeat_end])

                # By storing the positions of `EndingMarker` elements in
                # `endings`, the zero to three elements inbetween (as
                # returned by function `musicxml.Barline.to_lily_object`)
                # are removed after the following loop.  However, we have to
                # append the bar line elements (if present) since they might
                # further adjust the bars between endings by changing the
                # color, for example.
                #
                # In a similar vein, we have to prepend the data from
                # `<ending>` start elements to set properties for the
                # `VoltaSpanner` grobs.  Note that MusicXML doesn't provide
                # a separate `color` attribute for the volta number text.
                last_index = len(endings) - 1
                for i, (start, end) in enumerate(endings):
                    s = musicexp.SequentialMusic()

                    if isinstance(music_list[start], conversion.EndingMarker):
                        ending = music_list[start].mxl_event
                        attributes = ending._attribute_dict.copy()
                        attributes.pop('color', None)

                        v = musicexp.VoltaStyleEvent()
                        v.element = (ending, attributes)

                        if getattr(ending, 'print-object', 'yes') == 'no':
                            v.visible = False
                        v.color = getattr(ending, 'color', None)

                        s.elements.append(v)

                    s.elements.extend(music_list[start + 1:end])

                    if i < last_index:
                        for j in range(end + 1, endings[i + 1][0]):
                            if isinstance(music_list[j], musicexp.BarLine):
                                s.elements.append(music_list[j])
                                break
                    r.add_ending(s)

                del music_list[repeat_start:final_marker + 1]
                music_list.insert(repeat_start, r)
                repeat_replaced = True
            pos += 1
        # TODO: Implement repeats until the end without explicit ending bar
    return music_list


# Extract the settings for tuplets from the <notations><tuplet> and the
# <time-modification> elements of the note:
def musicxml_tuplet_to_lily(tuplet_elt, time_modification):
    tsm = musicexp.TimeScaledMusic()
    fraction = (1, 1)
    if time_modification:
        fraction = time_modification.get_fraction()
    tsm.numerator = fraction[0]
    tsm.denominator = fraction[1]

    (tsm.color, tsm.font_size) = tuplet_elt.get_tuplet_number_attributes()

    normal_type = tuplet_elt.get_normal_type()
    if not normal_type and time_modification:
        normal_type = time_modification.get_normal_type()
    if not normal_type and time_modification:
        note = time_modification.get_parent()
        if note:
            normal_type = note.get_duration_info()
    if normal_type:
        normal_note = musicexp.Duration()
        (normal_note.duration_log, normal_note.dots) = normal_type
        tsm.normal_type = normal_note

    actual_type = tuplet_elt.get_actual_type()
    if actual_type:
        actual_note = musicexp.Duration()
        (actual_note.duration_log, actual_note.dots) = actual_type
        tsm.actual_type = actual_note

    # Obtain non-default nrs of notes from the tuplet object!
    tsm.display_numerator = tuplet_elt.get_normal_nr()
    tsm.display_denominator = tuplet_elt.get_actual_nr()

    if getattr(tuplet_elt, 'bracket', None) == 'no':
        tsm.display_bracket = None
    elif getattr(tuplet_elt, 'line-shape', None) == 'curved':
        tsm.display_bracket = "curved"
    else:
        tsm.display_bracket = "bracket"

    display_values = {"none": None, "actual": "actual", "both": "both"}
    show_number = getattr(tuplet_elt, 'show-number', None)
    if show_number is not None:
        tsm.display_number = display_values.get(show_number, 'actual')

    show_type = getattr(tuplet_elt, 'show-type', None)
    if show_type is not None:
        tsm.display_type = display_values.get(show_type, None)

    if options.convert_directions:
        dir = getattr(tuplet_elt, 'placement', None)
        if dir is not None:
            tsm.force_direction = musicxml_direction_to_indicator(dir)

    return tsm


def group_tuplets(music_list, events):
    """Collect Musics from
    MUSIC_LIST demarcated by EVENTS_LIST in TimeScaledMusic objects.
    """

    indices = []
    brackets = {}

    j = 0
    for (ev_chord, tuplet_elt, time_modification, visible) in events:
        while j < len(music_list):
            # Since its registration in `events` the `ChordEvent` object
            # might be meanwhile wrapped into a two-stem tremolo or tuplet.
            if music_list[j].contains(ev_chord):
                break
            j += 1
        nr = getattr(tuplet_elt, 'number', 0)
        if tuplet_elt.type == 'start':
            tuplet_object = musicxml_tuplet_to_lily(
                tuplet_elt, time_modification)
            tuplet_object.visible = visible

            have_actual_normal = (tuplet_object.actual_type
                                  and tuplet_object.normal_type
                                  and tuplet_object.display_numerator
                                  and tuplet_object.display_denominator)

            if (isinstance(music_list[j], musicexp.RepeatedMusic)
                    and music_list[j].repeat_type == 'tremolo'):
                if have_actual_normal:
                    factor = ((tuplet_object.normal_type.get_length()
                               * tuplet_object.display_numerator)
                              / (tuplet_object.actual_type.get_length()
                                 * tuplet_object.display_denominator))
                    tuplet_object.numerator = factor.numerator
                    tuplet_object.denominator = factor.denominator
                else:
                    # There are no explicitly specified numerator and
                    # denumerator values, so adjust the time modification
                    # for the double-note tremolo.
                    tuplet_object.numerator *= 2
            elif have_actual_normal:
                tuplet_object.numerator = tuplet_object.display_numerator
                tuplet_object.denominator = tuplet_object.display_denominator

            tuplet_info = [j, -1, tuplet_object]
            indices.append(tuplet_info)
            brackets[nr] = tuplet_info
        elif tuplet_elt.type == 'stop':
            bracket_info = brackets.get(nr, None)
            # Ignore tuplet ends without corresponding starts.
            if bracket_info:
                bracket_info[1] = j  # Set the ending position to j
                del brackets[nr]

        # We don't increase `j` before the next loop since `music_list[j]`
        # might contain the end of a tuplet, too.

    # Sort `indices` by ascending start values as the primary and descending
    # end values as the secondary key.  This allows us to walk the list from
    # the start, processing the indices in linear order while popping off
    # completed tuplet groups.
    #
    # Indices example for a list with 27 entries:
    #
    #   ( 3, 11)
    #   ( 5,  9)
    #   (13, 24)
    #   (13, 17)
    #   (20, 24)
    #
    #   0-2               12                   25-26
    #      ↘             ↗  ↘                 ↗
    #       3-4     10-11   ↓      18-19      ↑
    #          ↘   ↗        ↘     ↗     ↘     ↗
    #           5-9          13-17       20-24
    from operator import itemgetter
    indices.sort(key=itemgetter(1), reverse=True)
    indices.sort(key=itemgetter(0))

    new_list = []
    out_stack = []
    out_stack.append(new_list)
    out = out_stack[-1]
    last = 0
    idx = 0
    while indices:
        (i1, i2, tsm) = indices[idx]
        if i1 > i2:
            # Ignore tuplet starts without corresponding ends.
            if len(out_stack) > 1:
                out_stack.pop()
            out = out_stack[-1]
            del indices[idx]
            if idx > 0:
                idx -= 1
            continue

        if last <= i1:
            # We have a new tuplet.
            out.extend(music_list[last:i1])
            out_stack.append([])
            out = out_stack[-1]
            last = i1

        if idx + 1 < len(indices) and i2 > indices[idx + 1][0]:
            # We have a nested tuplet.
            idx += 1
            continue
        else:
            i2 += 1
            # At this point, `music_list[last:i2]` encompasses all
            # (remaining) notes of the current tuplet.  There might be
            # dynamics following this range, however, which apply to the
            # last note of the tuplet.  Advance `i2` to include them in the
            # range.
            while (i2 < len(music_list)
                   and isinstance(music_list[i2], musicexp.DynamicsEvent)):
                i2 += 1
            if last < i2:
                out.extend(music_list[last:i2])

            seq = musicexp.SequentialMusic()
            seq.elements = out
            tsm.element = seq

            out_stack.pop()
            out = out_stack[-1]

            out.append(tsm)

            last = i2
            del indices[idx]
            if idx > 0:
                idx -= 1

    new_list.extend(music_list[last:])
    return new_list


def group_tremolos(music_list, events):
    left_idx = None
    right_idx = None

    num_beams = None
    num_strokes = None
    tremolo_color = None

    new_list = []

    last = 0
    j = 0
    for (ev_chord, tremolo_elt) in events:
        while j < len(music_list):
            if music_list[j] == ev_chord:
                break
            j += 1

        note = tremolo_elt.get_parent().get_parent().get_parent()

        if tremolo_elt.type == 'start':
            if left_idx is not None:
                ly.warning(_("Ignoring double-note tremolo without end"))

            left_idx = j

            beams = [b for b in note['beam']
                     if b.get_type() in ('begin', 'continue')]
            if beams:
                num_beams = max([int(b.number) for b in beams])
            else:
                num_beams = 0
            num_strokes = int(tremolo_elt.get_text())
            # `num_strokes` must be in the range [0;8]
            num_strokes = max(0, min(num_strokes, 8))

            # LilyPond can't set beam and tremolo stroke colors separately.
            # We first check for beam color, then for tremolo color.
            for b in beams:
                color = getattr(b, 'color', None)
                if color is not None:
                    tremolo_color = color
                    break
            if tremolo_color is None:
                color = getattr(tremolo_elt, 'color', None)
                if color is not None:
                    tremolo_color = color

            continue
        else:
            if left_idx is None:
                ly.warning(_("Ignoring double-note tremolo without start"))
                continue

            # We take all information on a double-stem tremolo from its left
            # element.
            right_idx = j

        # We found a double-note tremolo.
        #
        # Compute the values of `count` and `dur` as used in
        #
        #   \repeat tremolo <count> { left<dur> right<dur> }
        dur = 1 << (2 + num_beams + num_strokes)

        # We need the duration without the factor (i.e., without the
        # possible scaling caused by tuplets).
        length = music_list[left_idx].get_length(False) * dur / 2
        if length.denominator > 1:
            ly.warning(_("Strange tremolo note length encountered"))
        count = length.numerator

        # Add the factor again since `TimeScaledMusic` compensates it while
        # emitting durations.
        factor = (music_list[left_idx].get_length(True) * dur) / length
        duration = musicexp.Duration.from_fraction(Fraction(1, dur))
        duration.factor = factor

        # Adjust duration of chord notes.
        for i in [left_idx, right_idx]:
            chord = music_list[i]
            notes = [e for e in chord.elements
                     if isinstance(e, musicexp.NoteEvent)]
            for n in notes:
                n.duration = duration

        # At this point, `music_list[left_idx:right_idx]` encompasses the
        # two notes of the double-note tremolo.  There might be dynamics
        # following this range, however, which apply to the right note of
        # the tremolo (this doesn't make any sense under normal
        # circumstances, but who knows what the dynamics get used for).
        # Advance `right_idx` to include them in the range.
        while (right_idx < len(music_list)
               and isinstance(music_list[right_idx], musicexp.DynamicsEvent)):
            right_idx += 1

        new_list.extend(music_list[last:left_idx])

        r = musicexp.RepeatedMusic()
        r.repeat_type = "tremolo"
        r.repeat_count = count
        r.tremolo_strokes = num_strokes
        r.color = tremolo_color
        r.set_music(music_list[left_idx:(right_idx + 1)])

        new_list.append(r)

        last = right_idx + 1
        left_idx = None
        right_idx = None

    new_list.extend(music_list[last:])
    return new_list


def musicxml_clef_staff_details_to_lily(attributes):
    ev = musicexp.Clef_StaffLinesEvent()

    clef_information = attributes.get_clef_information()
    if clef_information is not None:
        (ev.type, ev.position, ev.octave, ev.color,
         ev.font_size, ev.visible) = clef_information

    stafflines = None

    details = attributes.get_maybe_exist_named_child('staff-details')
    if details:
        # TODO: Handle staff-type, staff-tuning, capo, staff-size
        stafflines = details.get_maybe_exist_named_child('staff-lines')
        if stafflines:
            ev.lines = int(stafflines.get_text())

            # TODO: Handle `color` attributes of staff lines individually.
            #       Handle `line-type`.
            line_details = details.get_named_children('line-detail')
            if line_details:
                ev.line_details = {}
                for line_detail in line_details:
                    line = int(getattr(line_detail, 'line', 1))
                    print_object = getattr(line_detail, 'print-object', 'yes')
                    if ev.lines_color is None:
                        ev.lines_color = getattr(line_detail, 'color', None)
                    ev.line_details[line] = print_object

    if clef_information is None and stafflines is None:
        return None

    # The percussion clef is a special case.
    if ev.type == 'percussion' or ev.type == 'PERC' or stafflines:
        needed_additional_definitions.append('staff-lines')

    return ev


def musicxml_time_to_lily(attributes):
    change = musicexp.TimeSignatureChange()
    # time signature function
    sig = attributes.get_time_signature().copy()
    if not sig:
        return None

    # 'Senza misura' time signature.
    if not isinstance(sig[0], list) and sig[0] < 0:
        change.fractions = [-sig[0], sig[1]]
        change.visible = False
        return change

    if options.shift_durations:
        if not isinstance(sig[0], list):
            sig = [sig]

        denominators = []
        for s in sig:
            denominators.append(s[-1])

        # Starting with python 3.9, `gcd` allows an arbitrary number of
        # arguments.
        gcd_denominator = gcd(*denominators)

        shift = options.shift_durations
        if shift < 0:
            denominator_shift = 0
            while shift < 0:
                # Only make the nominator larger if we no longer can make
                # the denominator smaller.
                if gcd_denominator % 2:
                    break
                gcd_denominator >>= 1
                denominator_shift += 1
                shift += 1

            nominator_shift = -shift

            for s in sig:
                s[:-1] = map(lambda x: x << nominator_shift, s[:-1])
                s[-1] >>= denominator_shift
        else:
            for s in sig:
                s[-1] <<= shift

        if isinstance(sig[0], list) and len(sig) == 1:
            sig = sig[0]

    change.fractions = sig

    time_elm = attributes.get_maybe_exist_named_child('time')
    symbol = getattr(time_elm, 'symbol', None)
    if symbol is not None:
        change.style = {'single-number': "'single-number",
                        'cut': None,
                        'common': None,
                        'normal': "'()"}.get(symbol, "'()")
    else:
        change.style = "'()"

    if getattr(time_elm, 'print-object', 'yes') == 'no':
        change.visible = False

    change.color = getattr(time_elm, 'color', None)
    change.font_size = getattr(time_elm, 'font-size', None)

    # TODO: Handle 'senza misura' symbol.
    # TODO: What shall we do if the symbol clashes with the sig? e.g. "cut"
    #       with 3/8 or "single-number" with(2+3)/8 or 3/8+2/4?
    return change


def musicxml_key_to_lily(attributes):
    key_signature = attributes.get_key_signature()
    if not key_signature:
        ly.warning(_("Unable to extract key signature!"))
        return None
    globvars.layout_information.set_context_item(
        'Staff', 'printKeyCancellation = ##f')

    (key_sig, color, font_size, visible) = key_signature

    change = musicexp.KeySignatureChange()
    change.color = color
    change.font_size = font_size
    change.visible = visible

    if len(key_sig) == 2 and not isinstance(key_sig[0], list):
        # standard key signature,(fifths, mode)
        (fifths, mode) = key_sig
        change.fifths = fifths
        change.mode = mode

        start_pitch = musicexp.Pitch()
        start_pitch.octave = 0
        try:
            (n, a) = {
                'major': (0, 0),
                'minor': (5, 0),
                'ionian': (0, 0),
                'dorian': (1, 0),
                'phrygian': (2, 0),
                'lydian': (3, 0),
                'mixolydian': (4, 0),
                'aeolian': (5, 0),
                'locrian': (6, 0),
            }[mode]
            start_pitch.step = n
            start_pitch.alteration = a
        except KeyError:
            ly.warning(_("unknown mode %s, expecting 'major' or 'minor' "
                         "or a church mode!") % mode)

        fifth = musicexp.Pitch()
        fifth.step = 4
        if fifths < 0:
            fifths *= -1
            fifth.step *= -1
            fifth.normalize()
        for x in range(fifths):
            start_pitch = start_pitch.transposed(fifth)
        change.tonic = start_pitch

    else:
        # Non-standard key signature of the form [[step,alter<,octave>],...]
        # MusicXML contains C,D,E,F,G,A,B as steps, lily uses 0-7, so convert
        alterations = []
        for k in key_sig:
            k[0] = conversion.musicxml_step_to_lily(k[0])
            alterations.append(k)
        change.non_standard_alterations = alterations

    cancel = attributes.get_cancellation()
    if cancel:
        (change.cancel_fifths, change.cancel_location) = cancel

        if change.cancel_location != 'left':
            needed_additional_definitions.append('insert-before')

        if change.cancel_location == 'before-barline':
            needed_additional_definitions.append('cancel-before-barline')
        elif change.cancel_location == 'right':
            needed_additional_definitions.append('cancel-after-key')

    return change


def musicxml_transpose_to_lily(attributes):
    transpose = attributes.get_transposition()
    if not transpose:
        return None

    shift = musicexp.Pitch()
    octave_change = transpose.get_maybe_exist_named_child('octave-change')
    if octave_change:
        shift.octave = int(octave_change.get_text())
    chromatic_shift = int(transpose.get_named_child('chromatic').get_text())
    chromatic_shift_normalized = chromatic_shift % 12
    (shift.step, shift.alteration) = [
        (0, 0), (0, 1), (1, 0), (2, -1), (2, 0),
        (3, 0), (3, 1), (4, 0), (5, -1), (5, 0),
        (6, -1), (6, 0)][chromatic_shift_normalized]

    shift.octave += (chromatic_shift - chromatic_shift_normalized) // 12

    diatonic = transpose.get_maybe_exist_named_child('diatonic')
    if diatonic:
        diatonic_step = int(diatonic.get_text()) % 7
        if diatonic_step != shift.step:
            # We got the alter incorrect!
            old_semitones = shift.semitones()
            shift.step = diatonic_step
            new_semitones = shift.semitones()
            shift.alteration += old_semitones - new_semitones

    transposition = musicexp.Transposition()
    transposition.pitch = musicexp.Pitch().transposed(shift)
    return transposition


def musicxml_measure_style_to_lily(attributes):
    details = attributes.get_named_children('measure-style')
    if not details:
        return None

    # TODO: Handle `measure-repeat`, `beat-repeat`, `slash`.
    ret = []
    for detail in details:
        color = getattr(detail, 'color', None)
        font_size = getattr(detail, 'font-size', None)

        multiple_rest = detail.get_maybe_exist_named_child('multiple-rest')
        if multiple_rest:
            measure_style_event = musicexp.MeasureStyleEvent()
            measure_style_event.color = color
            measure_style_event.font_size = font_size

            length = int(multiple_rest.get_text())
            measure_style_event.multiple_rest_length = length

            if getattr(multiple_rest, 'use-symbols', 'no') == 'yes':
                measure_style_event.use_symbols = True

            ret.append(measure_style_event)

    return ret


def musicxml_attributes_to_lily(attrs):
    elts = []
    # We handle `<clef>` and `<staff-details>` together for technical
    # reasons.
    attr_dispatch = [
        (('clef', 'staff-details'), musicxml_clef_staff_details_to_lily),
        ('time', musicxml_time_to_lily),
        ('key', musicxml_key_to_lily),
        ('transpose', musicxml_transpose_to_lily),
        ('measure-style', musicxml_measure_style_to_lily),
    ]
    for (k, func) in attr_dispatch:
        f = None
        if type(k) == tuple:
            children1 = attrs.get_named_children(k[0])
            children2 = attrs.get_named_children(k[1])
            if children1 or children2:
                f = func
        else:
            children = attrs.get_named_children(k)
            if children:
                f = func

        if f:
            ev = f(attrs)
            if ev is None:
                continue
            if isinstance(ev, list):
                elts.extend(ev)
            elif ev:
                elts.append(ev)

    return elts


def extract_display_text(el):
    children = el.get_typed_children(musicxml.get_class("display-text"))
    if children:
        return " ".join([child.get_text() for child in children])
    else:
        return False


def musicxml_print_to_lily(el):
    # TODO: Implement other print attributes
    #  <!ELEMENT print (page-layout?, system-layout?, staff-layout*,
    #          measure-layout?, measure-numbering?, part-name-display?,
    #          part-abbreviation-display?)>
    #  <!ATTLIST print
    #      staff-spacing %tenths; #IMPLIED
    #      new-system %yes-no; #IMPLIED
    #      new-page %yes-no-number; #IMPLIED
    #      blank-page NMTOKEN #IMPLIED
    #      page-number CDATA #IMPLIED
    #  >
    elts = []
    if conversion_settings.convert_system_breaks:
        if getattr(el, 'new-system', None) == 'yes':
            elts.append(musicexp.Break("break"))
    if conversion_settings.convert_page_breaks:
        if getattr(el, 'new-page', None) == 'yes':
            elts.append(musicexp.Break("pageBreak"))
    child = el.get_maybe_exist_named_child("part-name-display")
    if child:
        elts.append(musicexp.SetEvent("Staff.instrumentName",
                                      '"%s"' % extract_display_text(child)))
    child = el.get_maybe_exist_named_child("part-abbreviation-display")
    if child:
        elts.append(musicexp.SetEvent("Staff.shortInstrumentName",
                                      '"%s"' % extract_display_text(child)))
    return elts


spanner_event_dict = {
    'beam': musicexp.BeamEvent,
    'dashes': musicexp.TextSpannerEvent,
    'dynamics-spanner': musicexp.DynamicsSpannerEvent,
    'bracket': musicexp.BracketSpannerEvent,
    'glissando': musicexp.GlissandoEvent,
    'octave-shift': musicexp.OctaveShiftEvent,
    'pedal': musicexp.PedalEvent,
    'slide': musicexp.GlissandoEvent,
    'slur': musicexp.SlurEvent,
    'wavy-line': musicexp.TextSpannerEvent,
    'wedge': musicexp.HairpinEvent
}
spanner_type_dict = {
    'start': -1,
    'begin': -1,
    'crescendo': -1,
    'diminuendo': -1,
    'up': -1,
    'down': -1,

    'continue': 0,
    'change': 0,

    'stop': 1,
    'end': 1
    # TODO: 'backward hook' for <beam>
    # TODO: 'discontinue' for <pedal>
    # TODO: 'forward hook' for <beam>
    # TODO: 'let-ring' for <tied>
    # TODO: 'resume' for <pedal>
    # TODO: 'sostenuto' for <pedal>
}


def musicxml_spanner_to_lily_event(mxl_event, attributes=None,
                                   spanner_name=None, note_color=None,
                                   note_font_size=None):
    # The `note_color` and `note_font_size` arguments get ignored.

    ev = None

    if spanner_name is not None:
        name = spanner_name
    else:
        name = mxl_event.get_name()
    func = spanner_event_dict.get(name)
    if func:
        ev = func()
        mxl_event.spanner_event = ev
        ev.mxl_event = mxl_event
        ev.mxl_attributes = attributes
    else:
        ly.warning(_('unknown span event %s') % mxl_event)

    if name == "wavy-line":
        ev.style = ornament_has_what(ev, mxl_event)
    elif name == "dashes":
        ev.style = "dashes"
    elif name == "bracket":
        needed_additional_definitions.append("make-edge-height")

    type = mxl_event.get_type()
    span_direction = spanner_type_dict.get(type)
    if span_direction is not None:
        ev.span_direction = span_direction
    else:
        ly.warning(_('unknown span type %s for %s') % (type, name))

    if attributes is None:
        attributes = mxl_event._attribute_dict

    ev.set_span_type(type)
    ev.line_type = attributes.get('line-type',
                                  'wavy' if name == 'glissando' else 'solid')
    ev.start_stop = getattr(mxl_event, 'start_stop', False)

    # The `line-end` attribute gets handled in `BracketSpannerEvent`.

    ev.size = int(getattr(mxl_event, 'size', 0))  # attr of octave-shift
    ev.color = attributes.get('color', None)

    # The font size is handled via `text_to_ly()` in dynamics spanners.
    if name != 'dynamics-spanner':
        ev.font_size = attributes.get('font-size', None)

    if options.convert_directions:
        if span_direction == -1:
            # If both an associated ornament and the spanner has a
            # `placement` attribute, the former wins.
            try:
                dir = getattr(ev.mxl_ornament, 'placement', None)
            except AttributeError:
                dir = None

            if dir is None:
                dir = attributes.get('placement', None)
            if dir is not None:
                ev.force_direction = musicxml_direction_to_indicator(dir)

    return ev


def musicxml_direction_to_indicator(direction):
    return {"above": 1,
            "upright": 1,
            "up": 1,

            "below": -1,
            "downright": -1,
            "down": -1,
            "inverted": -1}.get(direction, 0)


def musicxml_fermata_to_lily_event(mxl_event, note_color=None,
                                   note_font_size=None):
    fermata_types = {
        '': 'fermata',
        'angled': 'shortfermata',
        # 'curlew': TODO,
        'double-angled': 'veryshortfermata',
        'double-dot': 'henzelongfermata',
        'double-squared': 'verylongfermata',
        'half-curve': 'henzeshortfermata',
        'normal': 'fermata',
        'square': 'longfermata',
    }

    ev = musicexp.ArticulationEvent()
    ev.type = fermata_types.get(mxl_event.get_text(), 'fermata')
    ev.color = getattr(mxl_event, 'color', note_color)
    ev.font_size = getattr(mxl_event, 'font-size', note_font_size)

    type_attr = getattr(mxl_event, 'type', None)
    if options.convert_directions and type_attr is not None:
        dir = musicxml_direction_to_indicator(type_attr)
        if dir:
            ev.force_direction = dir

    return ev


# Single-note tremolo.
def musicxml_tremolo_to_lily_event(mxl_event, note_color=None,
                                   note_font_size=None):
    ev = musicexp.TremoloEvent()
    ev.color = getattr(mxl_event, 'color', note_color)
    # TODO: Support unmeasured tremolos by handling the `smufl` attribute
    #       (which in turn would react to the currently unused `font-size`
    #       attribute).

    txt = mxl_event.get_text()
    if txt:
        ev.strokes = txt
    else:
        ev.strokes = 3
        ly.warning(_('empty <tremolo> element, setting value to %s')
                   % ev.strokes)
    return ev


def musicxml_falloff_to_lily_event(mxl_event, note_color=None,
                                   note_font_size=None):
    # The `note_font_size` argument gets ignored.
    ev = musicexp.BendEvent()
    ev.alter = -4
    ev.color = getattr(mxl_event, 'color', note_color)
    return ev


def musicxml_doit_to_lily_event(mxl_event, note_color=None,
                                note_font_size=None):
    # The `note_font_size` argument gets ignored.
    ev = musicexp.BendEvent()
    ev.alter = 4
    ev.color = getattr(mxl_event, 'color', note_color)
    return ev


def musicxml_bend_to_lily_event(mxl_event, note_color=None,
                                note_font_size=None):
    # The `note_font_size` argument gets ignored.
    ev = musicexp.BendEvent()
    ev.color = getattr(mxl_event, 'color', note_color)
    ev.alter = mxl_event.bend_alter()
    return ev


def musicxml_breath_mark_to_lily_event(mxl_event, note_color=None):
    # TODO: Read the <breath-mark-value> child and override the type
    # of symbol: comma, tick, upbow, salzedo.

    # TODO: Shall the color of `<note>` be inherited?
    # TODO: Shall the font size of `<note>` be inherited?
    color = getattr(mxl_event, 'color', None)
    font_size = getattr(mxl_event, 'font-size', None)
    ev = musicexp.BreatheEvent(color, font_size)
    return ev


def musicxml_caesura_to_lily_event(mxl_event, note_color=None):
    # TODO: Read the <caesura-value> child and override the type of
    # symbol: normal, thick, short, curved, single.

    # TODO: Shall the color of `<note>` be inherited?
    # TODO: Shall the font size of `<note>` be inherited?
    color = getattr(mxl_event, 'color', None)
    font_size = getattr(mxl_event, 'font-size', None)
    ev = musicexp.CaesuraEvent(color, font_size)
    return ev


def musicxml_fingering_event(mxl_event, note_color=None, note_font_size=None):
    ev = musicexp.FingeringEvent()
    ev.type = mxl_event.get_text()
    ev.alternate = getattr(mxl_event, 'alternate', 'no') == 'yes'
    ev.substitution = getattr(mxl_event, 'substitution', 'no') == 'yes'
    ev.color = getattr(mxl_event, 'color', note_color)
    ev.font_size = getattr(mxl_event, 'font-size', note_font_size)
    ev.visible = getattr(mxl_event, 'print-object', 'yes') == 'yes'

    if ev.substitution:
        needed_additional_definitions.append("fingering-substitution")

    return ev


def musicxml_pluck_event(mxl_event, note_color=None, note_font_size=None):
    ev = musicexp.FingeringEvent()
    ev.is_pluck = True
    ev.type = mxl_event.get_text()
    ev.color = getattr(mxl_event, 'color', note_color)
    ev.font_size = getattr(mxl_event, 'font-size', note_font_size)
    ev.visible = getattr(mxl_event, 'print-object', 'yes') == 'yes'

    needed_additional_definitions.append("pluck")

    return ev


def musicxml_string_event(mxl_event, note_color=None, note_font_size=None):
    ev = musicexp.NoDirectionArticulationEvent()
    ev.type = mxl_event.get_text()
    ev.color = getattr(mxl_event, 'color', note_color)
    ev.font_size = getattr(mxl_event, 'font-size', note_font_size)
    return ev


# This is for `<accidental-mark>` childs of `<notations>`.
def musicxml_accidental_mark(mxl_event, note_color=None, note_font_size=None):
    ev = musicexp.AccidentalMarkEvent()
    ev.contents = mxl_event.get_text()
    ev.color = getattr(mxl_event, 'color', note_color)
    ev.font_size = getattr(mxl_event, 'font-size', note_font_size)
    return ev


# Translate articulations, ornaments, and other notations into
# `ArticulationEvent` and similar objects.  Possible values:
#
#   -) string:           `ArticulationEvent` with that name
#   -) (string, string): `OrnamentEvent` with glyph name and its equivalent
#                        command
#   -) function:         `function(mxl_event)` needs to return a full
#                        `ArticulationEvent`-derived object
#   -) (class, name):    like 'string', only that a different class than
#                        `ArticulationEvent` is used
#
# TODO: Some translations are missing!
articulations_dict = {
    "accent": (musicexp.ShortArticulationEvent, ">"),  # or "accent"
    "accidental-mark": musicxml_accidental_mark,
    # "arrow": "?",
    "bend": musicxml_bend_to_lily_event,
    # "brass-bend": "?",
    "breath-mark": musicxml_breath_mark_to_lily_event,
    "caesura": musicxml_caesura_to_lily_event,
    # "delayed-inverted-turn": "?",
    # "delayed-turn": "?",
    "detached-legato": (musicexp.ShortArticulationEvent, "_"),  # or "portato"
    "doit": musicxml_doit_to_lily_event,
    # "double-tongue": "?",
    "down-bow": "downbow",
    "falloff": musicxml_falloff_to_lily_event,
    "fingering": musicxml_fingering_event,
    # "fingernails": "?",
    # "flip": "?",
    # "fret": "?",
    # "golpe": "?",
    # "half-muted": "?",
    # "hammer-on": "?",
    # "handbell": "?",
    # "harmon-mute": "?",
    # "harmonic": handled by `NoteEvent`,
    # "haydn": "?",
    # "heel": "?",
    # "hole": "?",
    "inverted-mordent": ("scripts.prall", "prall"),
    "inverted-turn": ("scripts.reverseturn", "reverseturn"),
    # "inverted-vertical-turn": "?",
    "mordent": ("scripts.mordent", "mordent"),
    # "open": "?",
    "open-string": "open",
    # "other-ornament": "?",
    # "other-technical": "?",
    # "plop": "?",
    "pluck": musicxml_pluck_event,
    # "pull-off": "?",
    # "schleifer": "?",
    # "scoop": "?",
    # "shake": "?",
    # "smear": "?",
    "snap-pizzicato": "snappizzicato",
    # "soft-accent": "?",
    # "spiccato": "?",
    "staccatissimo": (musicexp.ShortArticulationEvent, "!"),  # or "staccatissimo"
    "staccato": (musicexp.ShortArticulationEvent, "."),  # or "staccato"
    "stopped": (musicexp.ShortArticulationEvent, "+"),  # or "stopped"
    # "stress": "?",
    "string": musicxml_string_event,
    "strong-accent": (musicexp.ShortArticulationEvent, "^"),  # or "marcato"
    # "tap": "?",
    "tenuto": (musicexp.ShortArticulationEvent, "-"),  # or "tenuto"
    "thumb-position": "thumb",
    # "toe": "?",
    "turn": ("scripts.turn", "turn"),
    "tremolo": musicxml_tremolo_to_lily_event,  # only the single-note symbol
    "trill-mark": ("scripts.trill", "trill"),
    # "triple-tongue": "?",
    # "unstress": "?"
    "up-bow": "upbow",
    # "vertical-turn": "?",
    # "wavy-line": handled as spanner
}


def ornament_has_what(event, mxl_event):
    # In MusicXML, the trill symbol and the wavy line that follows are
    # handled separately, while in LilyPond they form a single grob,
    # `TrillSpanner`.  The code here checks whether `<trill-mark>` is
    # followed by (a starting) `<wavy-line>` and sets `mxl_ornament`
    # accordingly.

    wave = trill = None
    ignore = start = stop = False

    for i in mxl_event._parent._children:
        if i._name == "wavy-line":
            wave = i
        elif i._name == "trill-mark":
            trill = i

        try:
            if i.type == "continue":
                ignore = True
            elif i.type == "start":
                start = True
            elif i.type == "stop":
                stop = True
        except AttributeError:
            pass

    if start:
        if wave is not None and trill is not None:
            event.mxl_ornament = trill

    if ignore:
        return "ignore"
    elif stop:
        return "stop"
    elif wave is not None and trill is not None:
        return "trill and wave"
    elif wave is not None:
        return "wave"
    elif trill is not None:
        return "trill"


def OrnamenthasWavyline(mxl_event):
    for i in mxl_event._parent._children:
        if i._name == "wavy-line":
            return True
    return False


def musicxml_articulation_to_lily_event(mxl_event, note_color=None,
                                        note_font_size=None):
    name = mxl_event.get_name()
    if name == "wavy-line":
        # `wavy-line` elements are treated as trill spanners, not as
        # articulation ornaments.
        return musicxml_spanner_to_lily_event(mxl_event)
    elif name == "tremolo":
        # At this point, double-note `tremolo` elements have already been
        # handled.
        type = mxl_event.get_type()
        if type == 'start' or type == 'stop':
            return

    # A wavy line preceded by a trill mark gets handled as a spanner a few
    # lines above; if we see the `<trill-mark>` element, we thus pass.
    if OrnamenthasWavyline(mxl_event):
        return 'delayed'

    if name == 'harmonic':
        needed_additional_definitions.append('harmonic')

    tmp_tp = articulations_dict.get(name)
    if not tmp_tp:
        return 'unsupported'

    if isinstance(tmp_tp, str):
        ev = musicexp.ArticulationEvent()
        ev.type = tmp_tp
    elif isinstance(tmp_tp, tuple):
        if isinstance(tmp_tp[0], str):
            ev = musicexp.OrnamentEvent()
            # For accidental marks.
            ev.note_color = note_color
            ev.note_font_size = note_font_size
            ev.type = tmp_tp
        else:
            ev = tmp_tp[0]()
            ev.type = tmp_tp[1]
    else:
        ev = tmp_tp(mxl_event)

    ev.color = getattr(mxl_event, 'color', note_color)
    ev.font_size = getattr(mxl_event, 'font-size', note_font_size)

    # Some articulations use the type attribute, other the placement...
    if options.convert_directions:
        d = musicxml_direction_to_indicator(
            getattr(mxl_event, 'type', None)
            or getattr(mxl_event, 'placement', None))
        if d:
            ev.force_direction = d

    return ev


def musicxml_dynamic_to_lily(element):
    dynamics_name = element.get_name()
    if dynamics_name == 'other-dynamics':
        # TODO: Handle `smufl` attribute.
        dynamics_name = element.get_text()
    if not dynamics_name or dynamics_name == '#text':
        return ''
    else:
        return dynamics_name


def musicxml_dynamics_to_lily_event(elements, note_color=None,
                                    note_font_size=None):
    # A list of dynamics LilyPond provides by default.
    predefined_dynamics = (
        'ppppp', 'pppp', 'ppp', 'pp', 'p',
        'mp', 'mf',
        'f', 'ff', 'fff', 'ffff', 'fffff',
        'fp', 'sf', 'sfp', 'sff',
        'sfz', 'fz', 'sp', 'spp', 'rfz',
        'n'
    )

    # TODO: Shall the color of `<note>` be inherited?
    # TODO: Shall the font size of `<note>` be inherited?

    dyn_index = next(i for i, e in enumerate(elements)
                     if e[0].get_name() == 'dynamics')

    before_text_elements = elements[:dyn_index]
    after_text_elements = elements[(dyn_index + 1):]

    (dynamics, attributes) = elements[dyn_index]

    # Construct a name for the dynamics object.
    #
    # TODO: The code below is slightly problematic currently since we only
    #       take the `enclosure` attribute into account.  While in 'normal'
    #       scores it is unlikely to find, say, an 'f' in two different
    #       fonts (or colors, or sizes), this actually does happen in
    #       critical editions to make a distinction between original
    #       dynamics written by the composer and dynamics added by the
    #       editor.
    dynamics_name = ''

    before = ''
    for (e, _) in before_text_elements:
        before += e.get_text()
    dynamics_name += before

    # TODO: Handle font size.
    dyns = ''
    for d in dynamics.get_all_children():
        dyns += musicxml_dynamic_to_lily(d)
    dynamics_name += dyns

    after = ''
    for (e, _) in after_text_elements:
        after += e.get_text()
    dynamics_name += after

    enclosure = attributes.get('enclosure', 'none')
    if enclosure != 'none':
        dynamics_name += ' (' + enclosure + ')'

    dynamics_name = utilities.escape_ly_output_string(dynamics_name)
    dynamics_string = utilities.escape_ly_output_string(dyns)

    ev = musicexp.DynamicsEvent()

    # TODO: Handle more `attributes` elements.
    if dynamics_name in predefined_dynamics:
        ev.color = attributes.get('color', None)
        ev.font_size = attributes.get('font-size', None)
    else:
        if after or before or enclosure != 'none':
            markup = []
            markup_attributes = {}

            if before:
                markup.append(r'\dynamic')
            else:
                markup_attributes.update(attributes)
            markup.append(dynamics_string)
            if after:
                markup.append(r'\normal-text')

            markup_node = musicxml.LilyPond_markup()
            markup_node._data = ' '.join(markup)
            if enclosure != 'none':
                markup_attributes['enclosure'] = enclosure

            text_elements = []
            if before_text_elements:
                text_elements.extend(before_text_elements)
            text_elements.append((markup_node, markup_attributes))
            if after_text_elements:
                text_elements.extend(after_text_elements)

            init_markup = None
            if before:
                init_markup = r'\normal-text'
            dynamics_markup = musicexp.text_to_ly(text_elements, init_markup)

            additional_macros[dynamics_name] = (
                dynamics_name + ' =\n'
                + '#(make-dynamic-script #{\n'
                + '  \\markup {\n'
                + '    ' + dynamics_markup + '\n'
                + '  }\n'
                + '#})'
            )
        else:
            additional_macros[dynamics_name] = (
                dynamics_name
                + ' = #(make-dynamic-script "' + dynamics_string + '")'
            )
            ev.color = attributes.get('color', None)
            ev.font_size = attributes.get('font-size', None)

    ev.type = dynamics_name

    return ev


def musicxml_words_to_lily_event(elements):
    ev = musicexp.TextEvent()
    ev.text_elements = elements
    return ev


def musicxml_dashes_start_to_lily_event(elements):
    return musicxml_dashes_to_lily_event(elements, 'start')


def musicxml_dashes_stop_to_lily_event(elements):
    return musicxml_dashes_to_lily_event(elements, 'stop')


def musicxml_dashes_to_lily_event(elements, type):
    if type == 'start':
        (dashes, attributes) = elements[-1]
        elements = elements[:-1]
    else:
        (dashes, attributes) = elements[0]
        elements = elements[1:]

    ev = musicxml_spanner_to_lily_event(dashes, attributes)
    ev.text_elements = elements

    return ev


def musicxml_cresc_spanner_to_lily_event(elements):
    return musicxml_dynamics_spanner_to_lily_event(elements, 'cresc')


def musicxml_dim_spanner_to_lily_event(elements):
    return musicxml_dynamics_spanner_to_lily_event(elements, 'dim')


def musicxml_dynamics_spanner_to_lily_event(elements, type):
    (dynamics_spanner, attributes) = elements[-1]
    elements = elements[:-1]

    ev = musicxml_spanner_to_lily_event(dynamics_spanner, attributes,
                                        'dynamics-spanner')
    ev.text_elements = elements
    ev.type = type

    return ev


def musicxml_cresc_dim_stop_to_lily_event(elements):
    ev = musicexp.DynamicsSpannerEvent()
    ev.text_elements = elements
    ev.span_direction = 1
    return ev


def musicxml_mark_to_lily_event(elements):
    ev = musicexp.MarkEvent()
    ev.text_elements = elements
    return ev


def musicxml_textmark_to_lily_event(elements):
    globvars.layout_information.set_context_item(
        'Score', r'\override TextMark.font-size = 2')

    ev = musicexp.TextMarkEvent()
    ev.text_elements = elements
    return ev


# convert accordion-registration to lilypond.
# Since lilypond does not have any built-in commands, we need to create
# the markup commands manually and define our own variables.
# Idea was taken from: http://lsr.dsi.unimi.it/LSR/Item?id=194
def musicxml_accordion_to_markup(mxl_event):
    commandname = "accReg"
    command = ""

    high = mxl_event.get_maybe_exist_named_child('accordion-high')
    if high:
        commandname += "H"
        command += r"""\combine
          \raise #2.5 \musicglyph #"accordion.dot"
          """
    middle = mxl_event.get_maybe_exist_named_child('accordion-middle')
    if middle:
        # By default, use one dot (when no or invalid content is given). The
        # MusicXML spec is quiet about this case...
        txt = 1
        try:
            txt = int(middle.get_text())
        except ValueError:
            pass
        if txt == 3:
            commandname += "MMM"
            command += r"""\combine
          \raise #1.5 \musicglyph "accordion.dot"
          \combine
          \raise #1.5 \translate #(cons 1 0) \musicglyph "accordion.dot"
          \combine
          \raise #1.5 \translate #(cons -1 0) \musicglyph "accordion.dot"
          """
        elif txt == 2:
            commandname += "MM"
            command += r"""\combine
          \raise #1.5 \translate #(cons 0.5 0) \musicglyph "accordion.dot"
          \combine
          \raise #1.5 \translate #(cons -0.5 0) \musicglyph "accordion.dot"
          """
        elif not txt <= 0:
            commandname += "M"
            command += r"""\combine
          \raise #1.5 \musicglyph "accordion.dot"
          """
    low = mxl_event.get_maybe_exist_named_child('accordion-low')
    if low:
        commandname += "L"
        command += r"""\combine
          \raise #0.5 \musicglyph "accordion.dot"
          """

    command += r'\musicglyph "accordion.discant"'
    command = r"\markup { \normalsize %s }" % command
    # Define the newly built command \accReg[H][MMM][L]
    additional_macros[commandname] = '%s = %s' % (commandname, command)
    return r"\%s" % commandname


def musicxml_accordion_to_ly(mxl_event):
    txt = musicxml_accordion_to_markup(mxl_event)
    if txt:
        markup_node = musicxml.LilyPond_markup()
        markup_node._data = txt

        ev = musicexp.TextMarkEvent()
        ev.text_elements = [(markup_node, {})]

        return ev
    return


def musicxml_harp_pedals_to_ly(mxl_event):
    count = 0
    result = r'\harp-pedal #"'
    for t in mxl_event.get_named_children('pedal-tuning'):
        alter = t.get_named_child('pedal-alter')
        if alter:
            val = int(alter.get_text().strip())
            result += {1: "v", 0: "-", -1: "^"}.get(val, "")
        count += 1
        if count == 3:
            result += "|"
    ev = musicexp.MarkupEvent()
    ev.contents = result + '"'
    return ev


def musicxml_eyeglasses_to_ly(mxl_event):
    needed_additional_definitions.append("eyeglasses")

    markup_node = musicxml.LilyPond_markup()
    markup_node._data = r'\eyeglasses'

    ev = musicexp.TextMarkEvent()
    ev.text_elements = [(markup_node, {})]

    return ev


def next_non_hash_index(lst, pos):
    pos += 1
    while pos < len(lst) and isinstance(lst[pos], musicxml.Hash_text):
        pos += 1
    return pos


def musicxml_metronome_to_lily_event(elements):
    (maybe_metronome, attributes) = elements[-1]

    tempo_with_metronome = False
    children = None
    if isinstance(maybe_metronome, musicxml.Metronome):
        children = maybe_metronome.get_all_children()
        if children:
            tempo_with_metronome = True
        else:
            ly.warning(_("Empty metronome element"))

    ev = musicexp.TempoMark()

    if tempo_with_metronome:
        if attributes.get('parentheses', 'no') == 'yes':
            ev.parentheses = True
        if attributes.get('print-object', 'yes') == 'no':
            ev.visible = False
        # We extend MusicXML by accepting a carried-over `enclosure`
        # attribute for the metronome mark.
        enclosure_attribute = attributes.get('enclosure', 'none')
        if enclosure_attribute != 'none':
            ev.enclosure = enclosure_attribute

        if len(elements) > 1:
            ev.text_elements = elements[:-1]

        num_children = len(children)
        complex = False
        index = -1
        index = next_non_hash_index(children, index)
        if (index < num_children
                and isinstance(children[index], musicxml.BeatUnit)):
            while True:  # For flow control.
                d = None
                newd = None
                bpm = None

                # The simple form of a metronome mark.
                d = musicexp.Duration()
                d.duration_log = utilities.musicxml_duration_to_log(
                    children[index].get_text())
                index = next_non_hash_index(children, index)
                while (index < num_children
                       and isinstance(children[index], musicxml.BeatUnitDot)):
                    d.dots += 1
                    index = next_non_hash_index(children, index)

                if index >= num_children:
                    break

                if isinstance(children[index], musicxml.BeatUnitTied):
                    complex = True
                    break

                if isinstance(children[index], musicxml.BeatUnit):
                    # Form "note = newnote".
                    newd = musicexp.Duration()
                    newd.duration_log = utilities.musicxml_duration_to_log(
                        children[index].get_text())
                    index = next_non_hash_index(children, index)
                    while (index < num_children
                           and isinstance(children[index],
                                          musicxml.BeatUnitDot)):
                        newd.dots += 1
                        index = next_non_hash_index(children, index)

                    if (index < num_children
                            and isinstance(children[index],
                                           musicxml.BeatUnitTied)):
                        complex = True
                        break
                elif isinstance(children[index], musicxml.PerMinute):
                    # Form "note = bpm".
                    try:
                        bpm = int(children[index].get_text())
                    except ValueError:
                        ly.warning(_("Invalid bpm value in metronome mark"))
                        bpm = 0
                else:
                    ly.warning(_("Unknown metronome mark, ignoring"))
                    break

                ev.baseduration = d
                ev.newduration = newd
                ev.bpm = bpm
                break
        else:
            complex = True

        if complex:
            # TODO: Implement the other (more complex) way for tempo marks.
            ly.warning(_("Metronome marks with complex relations "
                         "(<metronome-note> in MusicXML) "
                         "are not yet implemented."))
    else:
        ev.text_elements = elements

    return ev


def musicxml_direction_to_lily(n):
    # TODO: Handle the `<staff>` element.
    res = []

    # The `placement` attribute applies to all children.
    dir = None
    if options.convert_directions:
        placement = getattr(n, 'placement', None)
        if placement is not None:
            dir = musicxml_direction_to_indicator(placement)

    directive = getattr(n, 'directive', 'no') == 'yes'

    dirtype_children = []
    attributes = {}

    for dt in n.get_typed_children(musicxml.DirType):
        dirtype_children += dt.get_all_children()
    dirtype_children = [d for d in dirtype_children if d.get_name() != "#text"]

    num_children = len(dirtype_children)
    i = 0
    while i < num_children:
        # We apply some heuristics to convert children of `<direction>` into
        # something meaningful.
        #
        # * In general, a `<dashes>` element, with `<words>` before a
        #   'start' element or `<words>` after a 'stop' element, translates
        #   to `\startTextSpan`.  However, if it is followed by
        #   `<dynamics>`, or if the text before the 'start' element contains
        #   either 'cresc', 'dim', or 'decresc', we translate it to a
        #   dynamics line spanner.
        #
        # * A single `<dynamics>` element, with `<words>` before or after,
        #   builds an argument to `make-dynamic-script`.
        #
        # * Multiple `<rehearsal>` elements in a row, with `<words>` before
        #   or after, become a `\mark` command.
        #
        # * `<segno>` and `<coda>` elements, with `<words>` elements before,
        #   inbetween, or after, build a `\textMark` markup.
        #
        # * A `<metronome>` element, with `<words>` before it, builds a
        #   `\tempo` command.
        #
        # * A series of only `<words>` elements builds a `\markup` command,
        #   except if `<direction>` has the attribute `directive="yes"`,
        #   which then makes it a `\tempo` command instead.
        #
        # TODO: Find a way to do something similar for horizontal brackets.
        # TODO: Handle `<symbol>` together with `<words>`.
        state_dict = {
            'cresc-dim-stop': musicxml_cresc_dim_stop_to_lily_event,
            'cresc-spanner': musicxml_cresc_spanner_to_lily_event,
            'dashes-start': musicxml_dashes_start_to_lily_event,
            'dashes-stop': musicxml_dashes_stop_to_lily_event,
            'dim-spanner': musicxml_dim_spanner_to_lily_event,
            'dynamics': musicxml_dynamics_to_lily_event,
            'mark': musicxml_mark_to_lily_event,
            'metronome': musicxml_metronome_to_lily_event,
            'textmark': musicxml_textmark_to_lily_event,
            'words': musicxml_words_to_lily_event,
        }
        # Contrary to other parts of MusicXML, "for a series of
        # `<direction-type>` children, non-positional formatting attributes
        # are carried over from previous elements by default."
        #
        # Unfortunately, it is not defined what 'non-positional formatting
        # attributes' actually means.  The following set of attributes to be
        # ignored for this 'carry-over' is thus a heuristic guess, combined
        # with attributes `musicxml2ly` doesn't handle.
        attributes_to_ignore = {
            'default-x',
            'default-y',
            'dir',  # not handled
            'id',  # not handled
            'relative-x',
            'relative-y',
            'smufl',
            'type',
            'xml:lang',  # not handled
        }
        direction_type_spanners = {
            'bracket',
            'octave-shift',
            'pedal',
            # 'principal-voice',
            'wedge',
        }
        direction_type_dict = {
            'accordion-registration': musicxml_accordion_to_ly,
            # 'damp': TODO
            # 'damp-all': TODO
            'eyeglasses': musicxml_eyeglasses_to_ly,
            'harp-pedals': musicxml_harp_pedals_to_ly,
            # 'image': TODO
            # 'other-direction': TODO
            # 'percussion': TODO
            # 'scordatura': TODO
            # 'staff-divide': TODO
            # 'string-mute': TODO
        }
        # A list of MusicXML enclosure types for which `musicxml2ly`
        # provides additional support.  Enclosure types unsupported by
        # LilyPond are filtered out in function `text_to_ly`.
        extra_enclosures = (
            'square'
        )

        entry = dirtype_children[i]

        # We store `<direction-type>` children together with the
        # carried-over attributes.
        elements = []

        rehearsal_enclosure_default = True
        maybe_dashes_stop_index = None
        maybe_dynamics_spanner = None

        n = i
        state = 'words'
        while n < num_children:
            elem = dirtype_children[n]
            name = elem.get_name()

            # We use `None` as the default so that we can check whether the
            # attribute is set at all.
            enclosure = getattr(elem, 'enclosure', None)
            if enclosure is not None:
                rehearsal_enclosure_default = False

            # Update attributes with data from current element.
            for a in elem._attribute_dict:
                if a not in attributes_to_ignore:
                    attributes[a] = elem._attribute_dict[a]

            if state == 'cresc-spanner':
                break
            elif state == 'dashes-start':
                break
            elif state == 'dashes-stop':
                if name == 'words':
                    pass
                elif name == 'dynamics':
                    n = maybe_dashes_stop_index + 1
                    break
                else:
                    break
            elif state == 'dim-spanner':
                break
            elif state == 'dynamics':
                if name == 'words':
                    pass
                else:
                    break
            elif state == 'cresc-dim-stop':
                break
            elif state == 'mark':
                if name == 'words':
                    if rehearsal_enclosure_default:
                        del attributes['enclosure']
                    state = 'post-mark'
                elif name == 'rehearsal':
                    pass
                else:
                    break
            elif state == 'post-mark':
                if name == 'words':
                    pass
                else:
                    break
            elif state == 'textmark':
                if name == 'words' or name == 'segno' or name == 'coda':
                    pass
                else:
                    break
            elif state == 'metronome':
                break
            elif state == 'words':
                if name == 'words':
                    # This is awkward, but MusicXML doesn't make a
                    # distinction whether dashes are used for, say, either
                    # 'cresc.' or 'rit.'.
                    cresc_re = r'(?x) (?<! \w) cresc'
                    dim_re = r'(?x) (?<! \w) ( decr | dim )'

                    text = elem.get_text()
                    if re.search(cresc_re, text):
                        maybe_dynamics_spanner = 'cresc'
                    elif re.search(dim_re, text):
                        maybe_dynamics_spanner = 'dim'

                    pass
                elif name == 'rehearsal':
                    if rehearsal_enclosure_default:
                        attributes['enclosure'] = 'square'
                    state = 'mark'
                elif name == 'segno' or name == 'coda':
                    state = 'textmark'
                elif name == 'dynamics':
                    state = 'dynamics'
                elif name == 'metronome':
                    state = 'metronome'
                elif name == 'dashes':
                    if elem.type == 'start':
                        if maybe_dynamics_spanner == 'cresc':
                            state = 'cresc-spanner'
                        elif maybe_dynamics_spanner == 'dim':
                            state = 'dim-spanner'
                        else:
                            state = 'dashes-start'
                    elif elem.type == 'stop':
                        state = 'dashes-stop'
                        maybe_dashes_stop_index = n
                        if elem.paired_with is not None:
                            paired = elem.paired_with.spanner_event
                            if type(paired) == musicexp.DynamicsSpannerEvent:
                                # Don't add `<words>` at the right of a
                                # dynamics spanner.
                                state = 'cresc-dim-stop'
                    else:
                        break
                else:
                    break

            enclosure = attributes.get('enclosure', None)
            if enclosure is not None and enclosure in extra_enclosures:
                needed_additional_definitions.append(enclosure)

            elements.append((elem, attributes.copy()))
            n += 1

        if state == 'words':
            if directive:
                state = 'metronome'
        elif state == 'post-mark':
            state = 'mark'

        if elements:
            event = state_dict[state](elements)
            if event:
                event.force_direction = dir
                res.append(event)
            i = n
            continue

        # At this point, the `attributes` array is up to date since the
        # start of the previous `while` loop gets always executed.

        if entry.get_name() in direction_type_spanners:
            event = musicxml_spanner_to_lily_event(entry)
            # TODO: Use `attributes`.
            if event:
                if event.span_direction == -1:
                    event.force_direction = dir
                res.append(event)
            i += 1
            continue

        # Everything else is taken as a single command (and ignored
        # otherwise if `musicxml2ly` can't handle it).
        dir_type_func = direction_type_dict.get(entry.get_name(), None)
        if dir_type_func:
            event = dir_type_func(entry)
            # TODO: Use `attributes`.
            if event:
                event.force_direction = dir
                res.append(event)

        i += 1

    return res


def musicxml_chordpitch_to_lily(mxl_cpitch):
    r = musicexp.ChordPitch()
    r.alteration = mxl_cpitch.get_alteration()
    r.step = conversion.musicxml_step_to_lily(mxl_cpitch.get_step())
    return r


chordkind_dict = {
    'major': '',
    'minor': ':m',
    'augmented': ':aug',
    'diminished': ':dim',
    # Sevenths:
    'dominant': ':7',
    'dominant-seventh': ':7',
    'major-seventh': ':maj7',
    'minor-seventh': ':m7',
    'diminished-seventh': ':dim7',
    'augmented-seventh': ':aug7',
    'half-diminished': ':m7.5-',
    'major-minor': ':maj7m',
    # Sixths:
    'major-sixth': ':6',
    'minor-sixth': ':m6',
    # Ninths:
    'dominant-ninth': ':9',
    'major-ninth': ':maj9',
    'minor-ninth': ':m9',
    # 11ths (usually as the basis for alteration):
    'dominant-11th': ':11',
    'major-11th': ':maj11',
    'minor-11th': ':m11',
    # 13ths (usually as the basis for alteration):
    'dominant-13th': ':13.11',
    'major-13th': ':maj13.11',
    'minor-13th': ':m13',
    # Suspended:
    'suspended-second': ':sus2',
    'suspended-fourth': ':sus4',
    # Functional sixths:
    # TODO
    # 'Neapolitan': '???',
    # 'Italian': '???',
    # 'French': '???',
    # 'German': '???',
    # Other:
    # 'pedal': '???',(pedal-point bass)
    'power': ':1.5',
    # 'Tristan': '???',
    'other': ':1',
    'none': None,
}


def musicxml_chordkind_to_lily(kind):
    res = chordkind_dict.get(kind, None)
    # Check for None, since a major chord is converted to ''
    if res is None:
        ly.warning(_("Unable to convert chord type %s to lilypond.") % kind)
    return res


# Global variable for guitar string tunings
string_tunings = None


def musicxml_get_string_tunings(lines):
    global string_tunings
    if string_tunings is None:
        if not lines:
            lines = 6
        string_tunings = [musicexp.Pitch()] * lines
        for i in range(0, lines):
            p = musicexp.Pitch()
            p.step = conversion.musicxml_step_to_lily(
                ((("E", "A", "D", "G", "B") * (lines / 5 + 1))[0:lines])[i])
            p.octave = (([-2 + int(x % 5 > 1) + 2 * (x / 5)
                          for x in range(0, lines)][0:lines])[i])
            p.alteration = 0
            p._force_absolute_pitch = True
            string_tunings[i] = p
        string_tunings = string_tunings[::-1]
    return string_tunings[0:lines]


def musicxml_frame_to_lily_event(frame):
    ev = musicexp.FretEvent()
    ev.strings = frame.get_strings()
    ev.frets = frame.get_frets()
    # offset = frame.get_first_fret() - 1
    # offset = frame.get_first_fret()
    barre = []
    open_strings = list(range(1, ev.strings + 1))
    for fn in frame.get_named_children('frame-note'):
        fret = fn.get_fret()
        if fret <= 0:
            fret = "o"
        el = [fn.get_string(), fret]
        fingering = fn.get_fingering()
        if fingering >= 0:
            el.append(fingering)
        ev.elements.append(el)
        open_strings.remove(fn.get_string())
        b = fn.get_barre()
        if b == 'start':
            barre.append(el[0])  # start string
            barre.append(el[1])  # fret
        elif b == 'stop':
            barre.insert(1, el[0])  # end string
    for string in open_strings:
        ev.elements.append([string, 'x'])
    ev.elements.sort()
    ev.elements.reverse()
    if barre:
        ev.barre = barre
    return ev


def musicxml_harmony_to_lily(n):
    res = []
    for f in n.get_named_children('frame'):
        ev = musicxml_frame_to_lily_event(f)
        if ev:
            res.append(ev)
    return res


def musicxml_harmony_to_lily_fretboards(n):
    res = []
    frame = n.get_maybe_exist_named_child('frame')
    if frame:
        strings = frame.get_strings()
        if not strings:
            strings = 6
        tunings = musicxml_get_string_tunings(strings)
        ev = musicexp.FretBoardEvent()
        # barre = []
        for fn in frame.get_named_children('frame-note'):
            fbn = musicexp.FretBoardNote()
            string = fn.get_string()
            fbn.string = string
            fingering = fn.get_fingering()
            if fingering >= 0:
                fbn.fingering = fingering
            p = tunings[string - 1].copy()
            p.add_semitones(fn.get_fret())
            fbn.pitch = p
            ev.append(fbn)
        res.append(ev)
    return res


def musicxml_harmony_to_lily_chordname(n):
    res = []
    root = n.get_maybe_exist_named_child('root')
    if root:
        ev = musicexp.ChordNameEvent()
        ev.root = musicxml_chordpitch_to_lily(root)
        kind = n.get_maybe_exist_named_child('kind')
        if kind:
            ev.kind = musicxml_chordkind_to_lily(kind.get_text())
            if ev.kind is None:
                return res
        bass = n.get_maybe_exist_named_child('bass')
        if bass:
            ev.bass = musicxml_chordpitch_to_lily(bass)
        inversion = n.get_maybe_exist_named_child('inversion')
        if inversion:
            # TODO: LilyPond does not support inversions, does it?

            # Mail from Carl Sorensen on lilypond-devel, June 11, 2008:
            # 4. LilyPond supports the first inversion in the form of added
            # bass notes.  So the first inversion of C major would be c:/g.
            # To get the second inversion of C major, you would need to do
            # e:6-3-^5 or e:m6-^5.  However, both of these techniques
            # require you to know the chord and calculate either the fifth
            # pitch (for the first inversion) or the third pitch (for the
            # second inversion) so they may not be helpful for musicxml2ly.
            inversion_count = int(inversion.get_text())
            if inversion_count == 1:
                # TODO: Calculate the bass note for the inversion...
                pass
            pass
        for deg in n.get_named_children('degree'):
            d = musicexp.ChordModification()
            d.type = deg.get_type()
            d.step = deg.get_value()
            d.alteration = deg.get_alter()
            ev.add_modification(d)
        # TODO: convert the user-symbols attribute:
            # major: a triangle, like Unicode 25B3
            # minor: -, like Unicode 002D
            # augmented: +, like Unicode 002B
            # diminished: (degree), like Unicode 00B0
            # half-diminished: (o with slash), like Unicode 00F8
        if ev and ev.root:
            res.append(ev)
    return res


def musicxml_figured_bass_note_to_lily(n):
    res = musicexp.FiguredBassNote()
    suffix_dict = {'sharp': "+",
                   'flat': "-",
                   'natural': "!",
                   'double-sharp': "++",
                   'flat-flat': "--",
                   'sharp-sharp': "++",
                   'slash': "/"}
    prefix = n.get_maybe_exist_named_child('prefix')
    if prefix:
        res.set_prefix(suffix_dict.get(prefix.get_text(), ""))
    fnumber = n.get_maybe_exist_named_child('figure-number')
    if fnumber:
        res.set_number(fnumber.get_text())
    suffix = n.get_maybe_exist_named_child('suffix')
    if suffix:
        res.set_suffix(suffix_dict.get(suffix.get_text(), ""))
    if n.get_maybe_exist_named_child('extend'):
        # TODO: Implement extender lines (unfortunately, in lilypond you have
        #       to use \set useBassFigureExtenders = ##t, which turns them on
        #       globally, while MusicXML has a property for each note...
        #       I'm not sure there is a proper way to implement this cleanly
        # n.extend
        pass
    return res


def musicxml_figured_bass_to_lily(n):
    if not isinstance(n, musicxml.FiguredBass):
        return
    res = musicexp.FiguredBassEvent()
    for i in n.get_named_children('figure'):
        note = musicxml_figured_bass_note_to_lily(i)
        if note:
            res.append(note)
    dur = n.get('duration')
    if dur is not None:
        # apply the duration to res
        length = Fraction(dur, n._divisions) * Fraction(1, 4)
        res.set_real_duration(length)
        res.set_duration(musicexp.Duration.from_fraction(length))
    if getattr(n, 'parentheses', None) == 'yes':
        res.set_parentheses(True)
    return res


def musicxml_lyrics_to_text(lyrics, ignoremelismata):
    # TODO: Handle `print-object`.
    continued = False
    extended = None

    lyric_color = getattr(lyrics, 'color', None)

    text = ''

    need_markup = False
    for e in lyrics.get_all_children():
        if ((isinstance(e, musicxml.Text) or isinstance(e, musicxml.Elision))
                and e._attribute_dict):
            need_markup = True
            break

    if need_markup:
        # Prepare input for `text_to_ly`.
        text_elements = []

        for e in lyrics.get_all_children():
            if isinstance(e, musicxml.Syllabic):
                continued = e.continued()
            elif isinstance(e, musicxml.Text):
                a = e._attribute_dict.copy()
                if 'color' not in a and lyric_color:
                    a['color'] = lyric_color

                w = musicxml.Words()
                # Convert soft hyphens to normal hyphens.
                w._data = e.get_text().replace('\u00AD', '-')

                text_elements.append((w, a))
            elif isinstance(e, musicxml.Elision):
                if text_elements:
                    if e.get_text() == '‿':  # U+203F UNDERTIE
                        a = e._attribute_dict.copy()
                        if 'color' not in a and lyric_color:
                            a['color'] = lyric_color

                        # LilyPond's support for `~` being replaced by a
                        # special undertie only works with text strings, not
                        # with markups.  Theoretically, we could implement
                        # the undertie construction in `musicxml2ly`, but...
                        w = musicxml.Words()
                        w._data = '‿'

                        text_elements.append((w, a))
                    else:
                        text_elements[-1][0]._data += ' '
                continued = False
            elif isinstance(e, musicxml.Extend):
                # If present it is the last element in `<lyric>`.
                extended = e
                break

        text = musicexp.text_to_ly(text_elements)
    else:
        for e in lyrics.get_all_children():
            if isinstance(e, musicxml.Syllabic):
                continued = e.continued()
            elif isinstance(e, musicxml.Text):
                # Convert soft hyphens to normal hyphens.
                text += e.get_text().replace('\u00AD', '-')
            elif isinstance(e, musicxml.Elision):
                if text:
                    if e.get_text() == '‿':  # U+203F UNDERTIE
                        text += '~'
                    else:
                        text += ' '
                continued = False
            elif isinstance(e, musicxml.Extend):
                # If present it is the last element in `<lyric>`.
                extended = e
                break

        w = musicxml.Words()
        w._data = text
        if lyric_color:
            a = {'color': lyric_color}
            need_markup = True
        else:
            a = {}
        text = musicexp.text_to_ly([(w, a)])

    # MusicXML 4.0 doesn't provide a way to change the appearance of hyphens
    # between syllables.
    hyphen = '--'
    extend = '__'

    if extended is not None:
        # Ignore 'continue' and 'stop' types (also for backward
        # compatibility with MusicXML 3.0).
        type = getattr(extended, 'type', None)
        if type == 'continue' or type == 'stop':
            extended = None
        else:
            extend_color = getattr(extended, 'color', lyric_color)
            color = musicexp.color_to_ly(extend_color)
            if color:
                extend = r'\tweak color %s __' % color

    # Using `-` and `_` as 'text markers' in addition to MusicXML elements
    # to indicate hyphens and extender lines for melismata, respectively, is
    # neither necessary nor covered by the standard.  However, it doesn't
    # harm to emit more `--` or `__` elements just in case since LilyPond
    # simply ignores them.
    if text == '"-"' and continued:
        return hyphen
    if text == '"_"' and extended:
        return extend

    if text == '""':
        text = ''

    if need_markup and text:
        text = r'\markup %s' % text

    if continued and text:
        if options.convert_beaming:
            if ignoremelismata == "on":
                return r' \set ignoreMelismata = ##t %s' % text
            elif ignoremelismata == "off":
                return r' %s %s \unset ignoreMelismata' % (text, hyphen)
            else:
                return r' %s %s' % (text, hyphen)
        else:
            return r' %s %s' % (text, hyphen)
    if continued:
        return hyphen

    if extended is not None and text:
        return r' %s %s' % (text, extend)
    if extended is not None:
        return extend

    if text:
        return r' %s' % text

    return ''


# TODO

class NegativeSkip:
    def __init__(self, here, dest):
        self.here = here
        self.dest = dest


class LilyPondVoiceBuilder(musicexp.Base):
    def __init__(self):
        self.elements = []
        self.pending_dynamics = []
        self.pending_last = []
        self.end_moment = 0
        self.begin_moment = 0
        self.pending_multibar = 0
        self.ignore_skips = False
        self.has_relevant_elements = False
        self.measure_length = 1  # As given by the measure's time signature.
        self.stay_here = False

    def contains(self, elem):
        if self == elem:
            return True
        for e in self.elements:
            if e.contains(elem):
                return True
        return False

    def _insert_multibar(self):
        globvars.layout_information.set_context_item(
            'Score', 'skipBars = ##t')
        globvars.layout_information.set_context_item(
            'Staff', r'\override MultiMeasureRest.expand-limit = 1')

        # Doing `R1^\markup{...}` would center the markup over the
        # multi-measure rest, which is most certainly not intended.
        # Instead, do `<>^\markup{...} R1`.  Since it doesn't cause a
        # problem, we do this for the remaining pending elements also.
        if self.pending_dynamics:
            self.elements.append(musicexp.EmptyChord())
            self.elements.extend(self.pending_dynamics)
            self.pending_dynamics = []

        r = musicexp.MultiMeasureRest()
        lenfrac = self.measure_length
        r.duration = musicexp.Duration.from_fraction(lenfrac)
        r.duration.factor *= self.pending_multibar / lenfrac
        self.elements.append(r)
        self.begin_moment = self.end_moment
        self.end_moment = self.begin_moment + self.pending_multibar
        self.pending_multibar = 0

    def set_measure_length(self, mlen):
        # `measure_length`, which is used to handle multi-measure rests,
        # completely ignores 'senza misura' time signatures – multi-measure
        # rests don't make sense with them.
        if mlen != self.measure_length and self.pending_multibar:
            self._insert_multibar()
        self.measure_length = mlen

    def add_multibar_rest(self, duration):
        self.pending_multibar += duration

    def set_duration(self, duration):
        self.end_moment = self.begin_moment + duration

    def current_duration(self):
        return self.end_moment - self.begin_moment

    def add_pending_dynamics(self):
        self.elements.extend(self.pending_dynamics)
        self.pending_dynamics = []

    def add_pending_last(self):
        # The elements in `pending_last` are separated from the previous
        # chord with `<>`.  We use this to implement support for
        # `<notation>` elements that start and stop at the same time step
        # (for example, a wavy line of a trill).  Normally, such elements
        # have different values of the `relative-x` property to indicate
        # their horizontal positions, but `relative-x` gets ignored by
        # `musicxml2ly`.
        #
        # Since `<>` must be inserted into the output after any
        # `<direction>` elements have been emitted, we handle this within
        # `LilyPondVoiceBuilder` (and not while constructing `ChordEvent`).
        self.elements.append(musicexp.EmptyChord())
        self.elements.extend(self.pending_last)
        self.pending_last = []

    def add_music(self, music, duration, relevant=True):
        assert isinstance(music, musicexp.Music)
        if self.pending_multibar > 0:
            self._insert_multibar()

        self.has_relevant_elements = self.has_relevant_elements or relevant

        if isinstance(music, musicexp.BarLine):
            if self.pending_dynamics:
                for d in self.pending_dynamics:
                    if not isinstance(
                            d, (musicexp.SpanEvent, musicexp.DynamicsEvent)):
                        index = self.pending_dynamics.index(d)
                        dyn = self.pending_dynamics.pop(index)
                        self.elements.append(dyn)

        # The elements in `pending_last` were added while processing the
        # previous `<note>` element and must be emitted before the current
        # `<note>` element gets handled.
        if isinstance(music, musicexp.ChordEvent) and self.pending_last:
            self.add_pending_last()

        self.elements.append(music)
        self.begin_moment = self.end_moment
        self.set_duration(duration)

        # Insert all pending dynamics right after the note/rest:
        if isinstance(music, musicexp.ChordEvent) and self.pending_dynamics:
            self.add_pending_dynamics()

    # Insert some music command that does not affect the position in the
    # measure.
    def add_command(self, command, relevant=True):
        assert isinstance(command, musicexp.Music)
        if self.pending_multibar > 0:
            self._insert_multibar()
        self.has_relevant_elements = self.has_relevant_elements or relevant
        self.elements.append(command)

    def add_barline(self, barline, bar_number, relevant=False):
        has_relevant = self.has_relevant_elements

        prev_barline = None
        if self.elements:
            if isinstance(self.elements[-1], musicexp.BarLine):
                prev_barline = self.elements[-1]

        if prev_barline is not None and self.pending_multibar == 0:
            # If we have an existing bar line object and no pending
            # multi-measure bar, set its bar number.
            prev_barline.bar_number = bar_number
        else:
            # Otherwise add a new bar line object.
            barline.bar_number = bar_number
            self.add_music(barline, 0)

        self.has_relevant_elements = has_relevant or relevant

    def add_partial(self, command):
        self.ignore_skips = True
        # insert the partial, but restore relevant_elements (partial is not
        # relevant)
        relevant = self.has_relevant_elements
        self.add_command(command)
        self.has_relevant_elements = relevant

    def add_dynamics(self, dynamic):
        # store the dynamic item(s) until we encounter the next note/rest:
        self.pending_dynamics.append(dynamic)

    def add_last(self, last):
        # Store items that come right before the next note (or bar line).
        self.pending_last.append(last)

    def add_bar_check(self, number):
        # (Re)store `has_relevant_elements` so that a barline alone does not
        # trigger output for figured bass or chord names.
        b = musicexp.BarLine()
        self.add_barline(b, number)

    def jumpto(self, moment):
        if self.stay_here:
            return

        current_end = self.end_moment + self.pending_multibar
        diff = moment - current_end
        if diff > 0:
            if self.ignore_skips and (moment == 0):
                # TODO: This seems convoluted and fragile.  LilyPond
                # regression tests still pass without this block, so what's
                # the problem?  Is test coverage lacking or is ignore_skips
                # bogus?
                self.ignore_skips = False
                return

            # TODO: Use the time signature for skips, too. Problem: The
            #       skip might not start at a measure boundary!
            skip = musicexp.SkipEvent()
            skip.duration.set_from_fraction(diff)

            evc = musicexp.ChordEvent()
            evc.elements.append(skip)
            self.add_music(evc, diff, False)

        elif diff < 0:
            ly.warning(_('Negative skip %s (from position %s to %s)') %
                        (diff, current_end, moment))

    def last_event_chord(self, starting_at):
        value = None

        # if the position matches, find the last ChordEvent, do not cross a
        # bar line!
        at = len(self.elements) - 1
        while (at >= 0
               and not isinstance(self.elements[at], musicexp.ChordEvent)
               and not isinstance(self.elements[at], musicexp.BarLine)):
            at -= 1

        if (self.elements
                and at >= 0
                and isinstance(self.elements[at], musicexp.ChordEvent)
                and self.begin_moment == starting_at):
            value = self.elements[at]
        else:
            self.jumpto(starting_at)
            value = None
        return value

    def correct_negative_skip(self, goto):
        self.end_moment = goto
        self.begin_moment = goto
        evc = musicexp.ChordEvent()
        self.elements.append(evc)


class VoiceData:
    def __init__(self):
        self.voicename = None
        self.voicedata = None
        self.ly_voice = None
        self.figured_bass = None
        self.chordnames = None
        self.fretboards = None
        self.lyrics_dict = {}
        self.lyrics_order = []


def measure_length_from_attributes(attr, current_measure_length):
    len = attr.get_measure_length()
    if not len or len < 0:
        len = current_measure_length
    return len


def music_xml_voice_name_to_lily_name(part_id, name):
    s = "Part%sVoice%s" % (part_id, name)
    return musicxml_id_to_lily(s)


def music_xml_lyrics_name_to_lily_name(part_id, name, lyricsnr):
    s = music_xml_voice_name_to_lily_name(
        part_id, name) + ("Lyrics%s" % lyricsnr)
    return musicxml_id_to_lily(s)


def music_xml_figuredbass_name_to_lily_name(part_id, voicename):
    s = music_xml_voice_name_to_lily_name(part_id, voicename) + "FiguredBass"
    return musicxml_id_to_lily(s)


def music_xml_chordnames_name_to_lily_name(part_id, voicename):
    s = music_xml_voice_name_to_lily_name(part_id, voicename) + "Chords"
    return musicxml_id_to_lily(s)


def music_xml_fretboards_name_to_lily_name(part_id, voicename):
    s = music_xml_voice_name_to_lily_name(part_id, voicename) + "FretBoards"
    return musicxml_id_to_lily(s)


def extract_lyrics(voice, lyric_key, lyrics_dict):
    def is_note(elem):
        return isinstance(elem, musicxml.Note)

    def is_rest(elem):
        return 'rest' in elem

    def is_chord(elem):
        return 'chord' in elem

    assert lyric_key is not None

    result = []

    base = None
    action = 'store'
    text = ''
    for elem in voice._elements:
        if not is_note(elem):
            continue

        lyrics = elem.get('lyric', [])

        if is_chord(elem):
            if is_rest(elem):
                type = 'chord rest'
            else:
                type = 'chord note'
        else:
            if is_rest(elem):
                type = 'main rest'
                if lyrics:
                    ly.warning(_('rests with lyrics are not supported yet'))
                    lyrics = []
            else:
                type = 'main note'

        if base == 'main rest':
            if type != 'main note':
                action = 'ignore'
        elif base == 'main note':
            if type == 'main rest' or type == 'main note':
                action = 'emit'

        if type == 'main rest' or type == 'main note':
            base = type

        if action == 'ignore':
            action = 'store'
            continue
        elif action == 'emit':
            if text:
                result.append(text)
                text = ''
            else:
                result.append(r' \skip1 ')

        for lyric in lyrics:
            # If there is more than a single entry with the same `number`
            # attribute, the existing one gets overwritten.  Note that we
            # ignore the `name` attribute.
            if getattr(lyric, 'number', '1') == lyric_key:
                text = musicxml_lyrics_to_text(lyric, None)

        action = 'store'

    if action == 'store':
        if base == 'main note':
            if text:
                result.append(text)
            else:
                result.append(r' \skip1 ')

    # We apply a heuristic regular expression to get a stanza number from
    # the first syllable; we search for strings like '1.', '1.-3.', '2.,
    # 3.', and '1./3./5.', with possible whitespace characters inbetween.
    stanza_id = None
    match = re.search(r'(?xs)'
                      r'^ \s*'
                      r'( " )?'
                      r'( (?: [0-9]+ \s* \. \s* [-/,]? \s* )+ ) \s*'
                      r'( [^-/,] .* )', result[0])
    if match:
        stanza_id = match.group(2).strip()
        result[0] = match.group(1) + match.group(3)

    lyrics_dict[lyric_key] = (result, stanza_id)


def musicxml_voice_to_lily_voice(voice):
    tremolo_events = []
    tuplet_events = []
    lyrics = {}
    return_value = VoiceData()
    return_value.voicedata = voice

    clef_visible = True
    key_visible = True
    note_visible = True

    # For `<unpitched>`.
    curr_clef = None

    # Track pitch alterations for cautionary accidentals without parentheses
    # (to be realized with LilyPond's `!` pitch modifier) that are not
    # represented with `<accidental cautionary="yes" parentheses="no">`.
    # Note that this might not work correctly if there are multiple voices
    # in a single staff.
    alterations = [0, 0, 0, 0, 0, 0, 0]
    curr_alterations = [0, 0, 0, 0, 0, 0, 0]

    # First pitch needed for relative mode (if selected in command-line
    # options)
    first_pitch = None

    # Needed for melismata detection (ignore lyrics on those notes!):
    inside_slur = False
    is_tied = False
    is_chord = False
    is_beamed = False
    ignore_lyrics = False

    # For pedal marks.
    pedal_is_line = False

    current_staff = None
    multibar_count = 0

    pending_figured_bass = []
    pending_chordnames = []
    pending_fretboards = []

    voice_builder = LilyPondVoiceBuilder()
    figured_bass_builder = LilyPondVoiceBuilder()
    chordnames_builder = LilyPondVoiceBuilder()
    fretboards_builder = LilyPondVoiceBuilder()
    current_measure_length = 1  # As given by the measure's time signature.
    voice_builder.set_measure_length(current_measure_length)
    in_slur = False

    # Make sure that the keys in the dict don't get reordered, since
    # we need the correct ordering of the lyrics stanzas! By default,
    # a dict will reorder its keys
    lyrics_numbers = voice.get_lyrics_numbers()
    return_value.lyrics_order = lyrics_numbers
    lyrics = {number: [] for number in lyrics_numbers}
    for number in lyrics_numbers:
        extract_lyrics(voice, number, lyrics)

    last_bar_check = -1
    senza_misura_time_signature = None
    for n in voice._elements:
        tie_started = False
        if n.get_name() == 'forward':
            continue

        staff_change = None
        staff = n.get('staff')
        if staff:
            if current_staff and staff != current_staff:
                staff_change = musicexp.StaffChange(staff)
                # A check for `<note>` follows later.
                if not isinstance(n, musicxml.Note):
                    voice_builder.add_command(staff_change)
                    staff_change = None
            current_staff = staff

        if isinstance(n, musicxml.Measure):
            if n.senza_misura_length:
                # Emission of this element must be delayed after a bar check
                # gets emitted.
                senza_misura_time_signature = musicexp.TimeSignatureChange()
                senza_misura_time_signature.visible = False
                senza_misura_time_signature.fractions = \
                    [n.senza_misura_length.numerator,
                     n.senza_misura_length.denominator]
            continue

        if isinstance(n, musicxml.Partial) and n.partial > 0:
            a = musicxml_partial_to_lily(n.partial)
            if a:
                voice_builder.add_partial(a)
                figured_bass_builder.add_partial(a)
                chordnames_builder.add_partial(a)
                fretboards_builder.add_partial(a)
            continue

        is_chord = 'chord' in n
        is_after_grace = (isinstance(n, musicxml.Note) and n.is_after_grace())
        if not is_chord and not is_after_grace:
            try:
                voice_builder.jumpto(n._when)
                figured_bass_builder.jumpto(n._when)
                chordnames_builder.jumpto(n._when)
                fretboards_builder.jumpto(n._when)
            except NegativeSkip as neg:
                voice_builder.correct_negative_skip(n._when)
                figured_bass_builder.correct_negative_skip(n._when)
                chordnames_builder.correct_negative_skip(n._when)
                fretboards_builder.correct_negative_skip(n._when)
                n.message(
                    _("Negative skip found: from %s to %s, difference is %s") %
                    (neg.here, neg.dest, neg.dest - neg.here))

        if isinstance(n, musicxml.Barline):
            barlines = n.to_lily_object()
            curr_alterations = alterations.copy()
            for a in barlines:
                if isinstance(a, musicexp.BarLine):
                    voice_builder.add_barline(a, 0)
                    figured_bass_builder.add_barline(a, 0, False)
                    chordnames_builder.add_barline(a, 0, False)
                    fretboards_builder.add_barline(a, 0, False)
                elif (isinstance(a, conversion.RepeatMarker)
                      or isinstance(a, conversion.EndingMarker)):
                    voice_builder.add_command(a)
                    figured_bass_builder.add_barline(a, 0, False)
                    chordnames_builder.add_barline(a, 0, False)
                    fretboards_builder.add_barline(a, 0, False)
            continue

        if isinstance(n, musicxml.Print):
            for a in musicxml_print_to_lily(n):
                voice_builder.add_command(a, False)
            continue

        # Continue any multimeasure-rests before trying to add bar checks!
        # Don't handle new MM rests yet, because for them we want bar checks!
        rest = n.get('rest')
        if (rest and rest.is_whole_measure()
                and multibar_count > 0
                and voice_builder.pending_multibar > 0):
            voice_builder.add_multibar_rest(n._duration)
            multibar_count -= 1
            continue

        # Print bar checks between measures.
        #
        # `_elements[0]` is always a `Measure` element that gets filtered
        # out above.
        if n._measure_position == 0 and n != voice._elements[1]:
            curr_alterations = alterations.copy()
            try:
                num = int(n.get_parent().number)
            except ValueError:
                num = 0
            # When we reach this point in the loop we are at the beginning
            # of a MusicXML measure.  However, we want to emit a bar check
            # *after* the measure, so we start with num == 2.
            if num > 1 and num > last_bar_check:
                voice_builder.add_bar_check(num)
                figured_bass_builder.add_bar_check(num)
                chordnames_builder.add_bar_check(num)
                fretboards_builder.add_bar_check(num)
                last_bar_check = num

        if (n._measure_position == 0
                and n != voice._elements[1]
                and senza_misura_time_signature):
            voice_builder.add_command(senza_misura_time_signature)
            senza_misura_time_signature = None

        if isinstance(n, musicxml.Direction):
            # check if Direction already has been converted in another voice.
            if n.converted:
                continue
            else:
                n.converted = True

                new_pedal_is_line = n.pedal_is_line()
                if (new_pedal_is_line is not None
                        and pedal_is_line != new_pedal_is_line):
                    style = "#'bracket" if new_pedal_is_line else "#'text"
                    se = musicexp.SetEvent("Staff.pedalSustainStyle", style)
                    voice_builder.add_command(se)
                    pedal_is_line = new_pedal_is_line

                for direction in musicxml_direction_to_lily(n):
                    if direction.wait_for_note():
                        voice_builder.add_dynamics(direction)
                    else:
                        voice_builder.add_command(direction)
                continue

        # Start any new multimeasure rests
        if rest and rest.is_whole_measure():
            if pending_chordnames:
                chordnames_builder.jumpto(n._when)
                chordnames_builder.stay_here = True
            if pending_figured_bass:
                figured_bass_builder.jumpto(n._when)
                figured_bass_builder.stay_here = True
            if pending_fretboards:
                fretboards_builder.jumpto(n._when)
                fretboards_builder.stay_here = True
            voice_builder.add_multibar_rest(n._duration)
            if multibar_count:
                multibar_count -= 1
            continue

        if isinstance(n, musicxml.Harmony):
            if options.fretboards:
                # Makes fretboard diagrams in a separate FretBoards voice
                for a in musicxml_harmony_to_lily_fretboards(n):
                    pending_fretboards.append(a)
            else:
                # Makes markup fretboard-diagrams inside the voice
                for a in musicxml_harmony_to_lily(n):
                    if a.wait_for_note():
                        voice_builder.add_dynamics(a)
                    else:
                        voice_builder.add_command(a)
            for a in musicxml_harmony_to_lily_chordname(n):
                pending_chordnames.append(a)
            continue

        if isinstance(n, musicxml.FiguredBass):
            a = musicxml_figured_bass_to_lily(n)
            if a:
                pending_figured_bass.append(a)
            continue

        if isinstance(n, musicxml.Attributes):
            for a in musicxml_attributes_to_lily(n):
                if isinstance(a, musicexp.KeySignatureChange):
                    alterations = a.get_alterations()
                    current_alterations = alterations

                    key_visible_new = a.visible

                    if key_visible and not key_visible_new:
                        voice_builder.add_command(
                            musicexp.OmitEvent('Staff.KeySignature'))
                        voice_builder.add_command(
                            musicexp.OmitEvent('Staff.KeyCancellation'))
                    elif not key_visible and key_visible_new:
                        voice_builder.add_command(
                            musicexp.OmitEvent('Staff.KeySignature',
                                               undo=True))
                        voice_builder.add_command(
                            musicexp.OmitEvent('Staff.KeyCancellation',
                                               undo=True))

                    key_visible = key_visible_new

                elif isinstance(a, musicexp.Clef_StaffLinesEvent):
                    curr_clef = a

                    clef_visible_new = a.visible

                    if a.type == 'none':
                        clef_visible_new = False

                    if clef_visible and not clef_visible_new:
                        voice_builder.add_command(
                            musicexp.OmitEvent('Staff.Clef'))
                    elif not clef_visible and clef_visible_new:
                        voice_builder.add_command(
                            musicexp.OmitEvent('Staff.Clef', undo=True))
                        voice_builder.add_command(
                            musicexp.SetEvent('Staff.forceClef', '##t',
                                              once=True))

                    clef_visible = clef_visible_new

                elif isinstance(a, musicexp.MeasureStyleEvent):
                    multibar_count = a.multiple_rest_length

                voice_builder.add_command(a)

            measure_length = measure_length_from_attributes(
                n, current_measure_length)
            if current_measure_length != measure_length:
                current_measure_length = measure_length
                voice_builder.set_measure_length(current_measure_length)
            continue

        if not n.__class__.__name__ == 'Note':
            n.message(_('unexpected %s; expected %s or %s or %s') %
                      (n, 'Note', 'Attributes', 'Barline'))
            continue

        is_double_note_tremolo = False

#        if not hasattr(conversion_settings, 'convert_rest_positions'):
#            conversion_settings.convert_rest_positions = True

        main_event = n.to_lily_object(
            curr_clef,
            convert_stem_directions=conversion_settings.convert_stem_directions,
            convert_rest_positions=conversion_settings.convert_rest_positions)

        if (isinstance(main_event, musicexp.NoteEvent)
                or isinstance(main_event, musicexp.RestEvent)):
            if not main_event.visible:
                note_visible = False
                if main_event.spacing:
                    needed_additional_definitions.append("hide-note")
            else:
                note_visible = True

        if isinstance(main_event, musicexp.NoteEvent):
            if not (main_event.cautionary or main_event.editorial):
                alteration = main_event.pitch.alteration
                step = main_event.pitch.step

                if curr_alterations[step] == alteration:
                    # Do we need a forced accidental?
                    if main_event.accidental_value:
                        main_event.forced_accidental = True
                else:
                    curr_alterations[step] = alteration

        # Do we need bracketed accidentals?
        if getattr(main_event, 'editorial', False):
            needed_additional_definitions.append("make-bracketed")

        if main_event and not first_pitch:
            first_pitch = main_event.pitch
        # ignore lyrics for notes inside a slur, tie, chord or beam
        ignore_lyrics = is_tied or is_chord  # or is_beamed or inside_slur

        # `ev_chord` starts as an empty `ChordEvent` object that gets filled
        # with items related to the current chord (notes, beams, etc.) while
        # iterating over elements of `voice`.
        ev_chord = voice_builder.last_event_chord(n._when)
        if not ev_chord:
            ev_chord = musicexp.ChordEvent()
            voice_builder.add_music(ev_chord, n._duration)

        # A staff change might happen anywhere; for this reason we have more
        # checks here and below.
        if staff_change and 'chord' in n:
            # TODO: Handle cross-staff chords.
            staff_change = None

        # For grace notes:
        grace = n.get('grace')
        if grace is not None:
            is_after_grace = ev_chord.has_elements() or n.is_after_grace()
            is_chord = 'chord' in n

            grace_chord = None

            # After-graces and other graces use different lists; depending
            # on whether we have a chord or not, obtain either a new
            # `ChordEvent` or the previous one to create a chord.
            if is_after_grace:
                if ev_chord.after_grace_elements and is_chord:
                    grace_chord = \
                        ev_chord.after_grace_elements.get_last_event_chord()
                if not grace_chord:
                    grace_chord = musicexp.ChordEvent()
                    if staff_change:
                        ev_chord.append_after_grace(staff_change)
                        staff_change = None
                    ev_chord.append_after_grace(grace_chord)
            else:
                if ev_chord.grace_elements and is_chord:
                    grace_chord = \
                        ev_chord.grace_elements.get_last_event_chord()
                if not grace_chord:
                    grace_chord = musicexp.ChordEvent()
                    if staff_change:
                        ev_chord.append_grace(staff_change)
                        staff_change = None
                    ev_chord.append_grace(grace_chord)

            if not is_after_grace:
                if getattr(grace, 'slash', None) == 'yes':
                    ev_chord.grace_type = "slashed"
            # Now that we have inserted the chord into the grace music,
            # insert everything into that chord instead of `ev_chord`.
            ev_chord = grace_chord
            ev_chord.append(main_event)
            ignore_lyrics = True
        else:
            if staff_change:
                ev_chord.append(staff_change)
                staff_change = None
            ev_chord.append(main_event)
            # When a note/chord has grace notes (duration==0), the duration
            # of the event chord is not yet known, but the event chord was
            # already added with duration 0.  The following corrects this
            # when we hit the real note!
            if voice_builder.current_duration() == 0 and n._duration > 0:
                voice_builder.set_duration(n._duration)

        # if we have a figured bass, set its voice builder to the correct
        # position and insert the pending figures
        if pending_figured_bass:
            try:
                figured_bass_builder.jumpto(n._when)
                if figured_bass_builder.stay_here:
                    figured_bass_builder.stay_here = False
            except NegativeSkip as neg:
                pass
            for fb in pending_figured_bass:
                # if a duration is given, use that, otherwise the one of the
                # note
                dur = fb.real_duration
                if not dur:
                    dur = ev_chord.get_length()
                if not fb.duration:
                    fb.duration = ev_chord.get_duration()
                figured_bass_builder.add_music(fb, dur)
            pending_figured_bass = []

        if pending_chordnames:
            try:
                chordnames_builder.jumpto(n._when)
                if chordnames_builder.stay_here:
                    chordnames_builder.stay_here = False
            except NegativeSkip as neg:
                pass
            for cn in pending_chordnames:
                # Assign the duration of the EventChord
                cn.duration = ev_chord.get_duration()
                chordnames_builder.add_music(cn, ev_chord.get_length())
            pending_chordnames = []

        if pending_fretboards:
            try:
                fretboards_builder.jumpto(n._when)
                if fretboards_builder.stay_here:
                    fretboards_builder.stay_here = False
            except NegativeSkip as neg:
                pass
            for fb in pending_fretboards:
                # Assign the duration of the EventChord
                fb.duration = ev_chord.get_duration()
                fretboards_builder.add_music(fb, ev_chord.get_length())
            pending_fretboards = []

        color = getattr(n, 'color', None)
        font_size = getattr(n, 'font-size', None)

        notations_children = n['notations']

        # The <notation> element can have the following children
        # (+ means implemented, ~ partially, - not):
        #
        # +tied | +slur | +tuplet | glissando | slide |
        #    ornaments | technical | articulations | dynamics |
        #    +fermata | arpeggiate | non-arpeggiate |
        #    accidental-mark | other-notation
        for notations in notations_children:
            for tuplet_event in notations.get_tuplets():
                time_mod = n.get_maybe_exist_typed_child(
                    musicxml.Time_modification)
                tuplet_events.append(
                    (ev_chord, tuplet_event, time_mod, note_visible))

            for arpeggiate in ['arpeggiate', 'non-arpeggiate']:
                for a in notations[arpeggiate]:
                    if not isinstance(ev_chord, musicexp.ArpeggioChordEvent):
                        ev_chord.__class__ = musicexp.ArpeggioChordEvent
                        ev_chord.init()

                        # Use first occurrence of the element to set
                        # attributes.
                        ev_chord.arpeggio = arpeggiate
                        ev_chord.arpeggio_dir = getattr(a, 'direction', None)
                        ev_chord.arpeggio_color = getattr(a, 'color', color)

                    # Setting a vertical minimum and a maximum position for
                    # the arpeggio is a subset of the theoretically possible
                    # chord configurations with `<arpeggiate>`.  However, it
                    # is already very rare that an arpeggio covers only a
                    # part of a chord so we keep it simple.
                    #
                    # The same holds (more or less) for `<non-arpeggiate>`.
                    # We only support one arpeggio bracket per chord (having
                    # more can be problematic as discussed in
                    # https://github.com/w3c/musicxml/discussions/540).
                    ev_chord.arpeggio_min_pitch = \
                        min(ev_chord.arpeggio_min_pitch,
                            main_event.pitch.steps())
                    ev_chord.arpeggio_max_pitch = \
                        max(ev_chord.arpeggio_max_pitch,
                            main_event.pitch.steps())

            # First, close all open slurs, only then start any new slur
            # TODO: Record the number of the open slur to determine the correct
            #       closing slur!
            endslurs = [s for s in notations['slur']
                        if s.get_type() in ('stop')]
            if endslurs and not inside_slur:
                endslurs[0].message(
                    _('Encountered closing slur, but no slur is open'))
            elif endslurs:
                if len(endslurs) > 1:
                    endslurs[0].message(
                        _('Cannot have two simultaneous (closing) slurs'))
                # record the slur status for the next note in the loop
                inside_slur = False
                lily_ev = musicxml_spanner_to_lily_event(endslurs[0])
                ev_chord.append(lily_ev)

            startslurs = [s for s in notations['slur']
                          if s.get_type() in ('start')]
            if startslurs and inside_slur:
                startslurs[0].message(
                    _('Cannot have a slur inside another slur'))
            elif startslurs:
                if len(startslurs) > 1:
                    startslurs[0].message(
                        _('Cannot have two simultaneous slurs'))
                # record the slur status for the next note in the loop
                inside_slur = True
                lily_ev = musicxml_spanner_to_lily_event(startslurs[0])
                lily_ev.visible = note_visible
                ev_chord.append(lily_ev)

            mxl_tie = notations.get_tie()
            if mxl_tie and mxl_tie.type == 'start':
                tie = musicexp.TieEvent()
                tie.color = getattr(mxl_tie, 'color', None)
                tie.visible = note_visible
                main_event.add_associated_event(tie)
                if not grace:
                    is_tied = True
                tie_started = True
            else:
                is_tied = False

            # Articulations can contain the following child elements:
            #         accent | strong-accent | staccato | tenuto |
            #         detached-legato | staccatissimo | spiccato |
            #         scoop | plop | doit | falloff | breath-mark |
            #         caesura | stress | soft-accent | unstress
            # Technical can contain the following child elements:
            #         up-bow | down-bow | harmonic | open-string |
            #         thumb-position | fingering | pluck | double-tongue |
            #         triple-tongue | stopped | snap-pizzicato | fret |
            #         string | hammer-on | pull-off | bend | tap | heel |
            #         toe | fingernails | hole | arrow | handbell |
            #         brass-bend | flip | smear | open | half-muted |
            #         harmon-mute | golpe | other-technical
            # Ornaments can contain the following child elements:
            #         trill-mark | turn | delayed-turn | inverted-turn |
            #         delayed-inverted-turn | vertical-turn |
            #         inverted-vertical-turn | shake | wavy-line |
            #         mordent | inverted-mordent | schleifer | tremolo |
            #         haydn | other-ornament,
            #         accidental-mark
            def convert_and_append_all_child_articulations(
                    mxl_node, note_color=None, note_font_size=None):
                # Mark trill spanners where `start` and `stop` elements (in
                # that order) happen at the same musical moment.
                res = []
                wavy_line_starts = []
                for ch in mxl_node.get_named_children('wavy-line'):
                    id = getattr(ch, 'number', 1)
                    type = getattr(ch, 'type', None)
                    if type == 'start':
                        wavy_line_starts.append(id)
                    elif type == 'stop':
                        if id in wavy_line_starts:
                            ch.start_stop = True

                # Double-note tremolos.
                #
                # Note that LilyPond can't handle tremolo beams if a beam
                # has already started (issue #6706); the code output by
                # `musicxml2ly` causes warnings and incorrect rendering
                # results that have to be resolved manually.
                for ch in mxl_node.get_named_children('tremolo'):
                    type = getattr(ch, 'type', None)
                    if type == 'start' or type == 'stop':
                        # No need to take care of `<time-modification>`
                        # elements; we always halve the duration of the
                        # affected notes.
                        tremolo_events.append((ev_chord, ch))

                        nonlocal is_double_note_tremolo
                        is_double_note_tremolo = True

                ev = None
                delayed_accidental_marks = []

                for ch in mxl_node.get_all_children():
                    if isinstance(ch, musicxml.Hash_text):
                        continue

                    if ch._name == 'accidental-mark':
                        if ev == 'unsupported':
                            # Silently ignore accidental marks attached to
                            # unhandled ornaments.
                            continue
                        elif ev == 'delayed':
                            delayed_accidental_marks.append(ch)
                            continue

                        try:
                            ev.accidental_marks.append(ch)
                            needed_additional_definitions.append(
                                'accidental-marks')
                        except AttributeError:
                            ly.warning(_('ignoring <accidental-mark> not '
                                         'attached to proper <ornaments> '
                                         'child'))
                        continue

                    ev = musicxml_articulation_to_lily_event(ch, note_color,
                                                             note_font_size)
                    if (ev is not None
                            and ev != 'unsupported' and ev != 'delayed'):
                        try:
                            if delayed_accidental_marks:
                                ev.accidental_marks.extend(
                                    delayed_accidental_marks)
                                needed_additional_definitions.append(
                                    'accidental-marks')
                        except AttributeError:
                            pass

                        try:
                            if ev.start_stop == True:
                                voice_builder.add_last(ev)
                                continue
                        except AttributeError:
                            pass

                        res.append(ev)

                return res

            def convert_and_append_all_child_dynamics(mxl_node,
                                                      note_color=None,
                                                      note_font_size=None):
                element = (mxl_node, mxl_node._attribute_dict)
                ev = musicxml_dynamics_to_lily_event([element], note_color,
                                                     note_font_size)
                if ev is not None:
                    if options.convert_directions:
                        dir = getattr(mxl_node, 'placement', None)
                        if dir is not None:
                            ev.force_direction = \
                                musicxml_direction_to_indicator(dir)
                    ev_chord.append(ev)

            notation_handlers = {
                'accidental-mark': musicxml_articulation_to_lily_event,
                'articulations': convert_and_append_all_child_articulations,
                'dynamics': convert_and_append_all_child_dynamics,
                'fermata': musicxml_fermata_to_lily_event,
                'glissando': musicxml_spanner_to_lily_event,
                'ornaments': convert_and_append_all_child_articulations,
                'slide': musicxml_spanner_to_lily_event,
                'technical': convert_and_append_all_child_articulations,
            }

            for a in notations.get_all_children():
                handler = notation_handlers.get(a.get_name(), None)
                if handler is not None:
                    ev = handler(a, note_color=color,
                                 note_font_size=font_size)
                    if not isinstance(ev, list):
                        ev = [ev]
                    for e in ev:
                        if isinstance(e, musicexp.SpanEvent):
                            e.visible = note_visible

                        if isinstance(e, musicexp.FingeringEvent):
                            main_event.add_associated_event(e)
                        else:
                            ev_chord.append(e)

        mxl_beams = [b for b in n['beam']
                     if (b.get_type() in ('begin', 'end')
                         and b.is_primary()
                         and not is_double_note_tremolo)]
        if mxl_beams and not conversion_settings.ignore_beaming:
            beam_ev = musicxml_spanner_to_lily_event(mxl_beams[0])
            if beam_ev:
                ev_chord.append(beam_ev)
                if beam_ev.span_direction == -1:
                    # beam and thus melisma starts here
                    is_beamed = True
                elif beam_ev.span_direction == 1:
                    # beam and thus melisma ends here
                    is_beamed = False

        # Assume that a <tie> element only lasts for one note.
        # This might not be correct MusicXML interpretation, but works for
        # most cases and fixes broken files, which have the end tag missing
        if is_tied and not tie_started:
            is_tied = False

    # force trailing mm rests to be written out.
    voice_builder.add_music(musicexp.ChordEvent(), 0)

    ly_voice = group_tremolos(voice_builder.elements, tremolo_events)
    ly_voice = group_tuplets(ly_voice, tuplet_events)
    ly_voice = group_repeats(ly_voice)

    seq_music = musicexp.SequentialMusic()

    seq_music.elements = ly_voice
    for k, v in lyrics.items():
        ev = musicexp.Lyrics()
        ev.lyrics_syllables = v[0]
        ev.stanza_id = v[1]
        return_value.lyrics_dict[k] = ev

    if options.shift_durations:
        sd = musicexp.ShiftDurations()
        sd.element = seq_music
        seq_music = sd

    if options.relative:
        v = musicexp.RelativeMusic()
        v.element = seq_music
        v.basepitch = first_pitch
        seq_music = v

    return_value.ly_voice = seq_music

    # create \figuremode { figured bass elements }
    if figured_bass_builder.has_relevant_elements:
        fbass_music = musicexp.SequentialMusic()
        fbass_music.elements = group_repeats(figured_bass_builder.elements)
        v = musicexp.ModeChangingMusicWrapper()
        v.mode = 'figuremode'
        v.element = fbass_music
        if options.shift_durations:
            sd = musicexp.ShiftDurations()
            sd.element = v
            v = sd
        return_value.figured_bass = v

    # create \chordmode { chords }
    if chordnames_builder.has_relevant_elements:
        cname_music = musicexp.SequentialMusic()
        cname_music.elements = group_repeats(chordnames_builder.elements)
        v = musicexp.ModeChangingMusicWrapper()
        v.mode = 'chordmode'
        v.element = cname_music
        if options.shift_durations:
            sd = musicexp.ShiftDurations()
            sd.element = v
            v = sd
        return_value.chordnames = v

    # create diagrams for FretBoards engraver
    if fretboards_builder.has_relevant_elements:
        fboard_music = musicexp.SequentialMusic()
        fboard_music.elements = group_repeats(fretboards_builder.elements)
        v = musicexp.MusicWrapper()
        v.element = fboard_music
        if options.shift_durations:
            sd = musicexp.ShiftDurations()
            sd.element = v
            v = sd
        return_value.fretboards = v

    # coll = []
    # pending = []

    # for elt in return_value.ly_voice.element.elements:
    #     if isinstance(elt, musicexp.TimeScaledMusic):
    #         print elt.element.elements
    #         pending.append(elt)
    #     else:
    #         coll.append(elt)

    # if pending:
    #     coll.extend(pending)

    # return_value.ly_voice.element.elements = coll

    return return_value


def musicxml_id_to_lily(id):
    digits = ['Zero', 'One', 'Two', 'Three', 'Four', 'Five',
              'Six', 'Seven', 'Eight', 'Nine', 'Ten']

    for digit in digits:
        d = digits.index(digit)
        id = re.sub('%d' % d, digit, id)

    id = re.sub('[^a-zA-Z]', 'X', id)
    return id


def voices_in_part(part):
    """Return a Name -> Voice dictionary for PART"""
    part.interpret()
    part.extract_voices()
    voices = part.get_voices()
    part_info = part.get_staff_attributes()

    return (voices, part_info)


def voices_in_part_in_parts(parts):
    """return a Part -> Name -> Voice dictionary"""
    dictionary = {}
    for p in parts:
        voices = voices_in_part(p)
        # don't crash if Part doesn't have an id (that's invalid MusicXML,
        # but such files are out in the wild!)
        # TODO: extract correct part id from other sources
        part_id = getattr(p, 'id', None)
        dictionary[part_id] = voices
    return dictionary


def get_all_voices(parts):
    all_voices = voices_in_part_in_parts(parts)

    all_ly_voices = {}
    all_ly_staffinfo = {}
    for p, (name_voice, staff_info) in list(all_voices.items()):
        part_ly_voices = OrderedDict()
        for n, v in list(name_voice.items()):
            ly.progress(_("Converting to LilyPond expressions..."), True)
            # musicxml_voice_to_lily_voice returns
            # (lily_voice, {nr->lyrics, nr->lyrics})
            voice = musicxml_voice_to_lily_voice(v)
            part_ly_voices[n] = voice

        all_ly_voices[p] = part_ly_voices
        all_ly_staffinfo[p] = staff_info

    return (all_ly_voices, all_ly_staffinfo)


def option_parser():
    p = ly.get_option_parser(usage=_("musicxml2ly [OPTION]... FILE.xml"),
                             description=_("""\
Convert MusicXML from FILE.xml to LilyPond input.
If the given filename is -, musicxml2ly reads from the command line.
"""),
                             add_help_option=False)

    p.add_option("-h", "--help",
                 action="help",
                 help=_("show this help and exit"))

    p.version = ('%prog (LilyPond) ' + lilypond_version + '\n\n'
                 +
                 _("""Copyright (c) 2005--2023 by
    Han-Wen Nienhuys <hanwen@xs4all.nl>,
    Jan Nieuwenhuizen <janneke@gnu.org> and
    Reinhold Kainhofer <reinhold@kainhofer.com>
    Patrick L. Schmidt <pls@philomelos.net>
"""
                   +
                   """
This program is free software.  It is covered by the GNU General Public
License and you are welcome to change it and/or distribute copies of it
under certain conditions.  Invoke as `%s --warranty' for more
information.""") % 'lilypond')

    p.add_option("--version",
                 action="version",
                 help=_("show version number and exit"))

    p.add_option('-v', '--verbose',
                 action="callback",
                 callback=ly.handle_loglevel_option,
                 callback_args=("DEBUG",),
                 help=_("be verbose"))

    p.add_option('', '--lxml',
                 action="callback",
                 callback=lambda option, opt_str, value, parser:
                 ly.warning(_('--lxml is obsolete, ignoring')),
                 help=_("obsolete"))

    p.add_option('-z', '--compressed',
                 action="store_true",
                 dest='compressed',
                 default=False,
                 help=_("input file is a compressed MusicXML file "
                        "(by default, activate if file extension is .mxl)"))

    p.add_option('-r', '--relative',
                 action="store_true",
                 default=True,
                 dest="relative",
                 help=_("convert pitches in relative mode (default)"))

    p.add_option('-a', '--absolute',
                 action="store_false",
                 dest="relative",
                 help=_("convert pitches in absolute mode"))

    p.add_option('-l', '--language',
                 metavar=_("LANG"),
                 action="store",
                 help=_("use LANG for pitch names, "
                        "e.g., 'deutsch' for note names in German"))

    p.add_option("--loglevel",
                 help=_("Print log messages according to LOGLEVEL "
                        "(NONE, ERROR, WARN, PROGRESS (default), DEBUG)"),
                 metavar=_("LOGLEVEL"),
                 action='callback',
                 callback=ly.handle_loglevel_option,
                 type='string')

    p.add_option('--nd', '--no-articulation-directions',
                 action="store_false",
                 default=True,
                 dest="convert_directions",
                 help=_("do not convert directions (^, _ or -) "
                        "for articulations, dynamics, etc."))

    p.add_option('--nrp', '--no-rest-positions',
                 action="store_false",
                 default=True,
                 dest="convert_rest_positions",
                 help=_("do not convert exact vertical positions of rests"))

    p.add_option('--nsb', '--no-system-breaks',
                 action="store_false",
                 default=True,
                 dest="convert_system_breaks",
                 help=_("ignore system breaks"))

    p.add_option('--npb', '--no-page-breaks',
                 action="store_false",
                 default=True,
                 dest="convert_page_breaks",
                 help=_("ignore page breaks"))

    p.add_option('--npm', '--no-page-margins',
                 action="store_false",
                 default=True,
                 dest="convert_page_margins",
                 help=_("ignore page margins"))

    p.add_option('--npl', '--no-page-layout',
                 action="store_false",
                 default=True,
                 dest="convert_page_layout",
                 help=_("do not convert the exact page layout and breaks "
                        '(shortcut for "--nsb --npb --npm" options)'))

    p.add_option('--nsd', '--no-stem-directions',
                 action="store_false",
                 default=True,
                 dest="convert_stem_directions",
                 help=_("ignore stem directions from MusicXML, "
                        "use lilypond's automatic stemming instead"))

    p.add_option('--afs', '--absolute-font-sizes',
                 action="store_true",
                 default=False,
                 dest="absolute_font_sizes",
                 help=_("use absolute font sizes in markup "
                        "(default is to use font sizes based on score size)"))

    p.add_option('--nb', '--no-beaming',
                 action="store_false",
                 default=True,
                 dest="convert_beaming",
                 help=_("do not convert beaming information, "
                        "use lilypond's automatic beaming instead"))

    p.add_option('-o', '--output',
                 metavar=_("FILE"),
                 action="store",
                 default=None,
                 type='string',
                 dest='output_name',
                 help=_("set output filename to FILE, stdout if -"))

    p.add_option('-m', '--midi',
                 action="store_true",
                 default=False,
                 dest="midi",
                 help=_("activate midi-block in .ly file"))

    # transpose function
    p.add_option('--transpose',
                 metavar=_("TOPITCH"),
                 action="store",
                 dest="transpose",
                 help=_("set pitch to transpose by the interval "
                        "between pitch 'c' and TOPITCH"))

    # time signature changing function
    p.add_option('--sd', '--shift-durations',
                 metavar=_('VALUE'),
                 action='store',
                 dest='shift_durations',
                 default=0,
                 type='int',
                 help=_('shift durations and time signatures by VALUE; '
                        'for example, value -1 doubles all durations, '
                        'and value 1 halves them'))

    p.add_option('--sm', '--shift-meter',
                 metavar=_("BEATS/BEATTYPE"),
                 action='callback',
                 dest='shift_meter',
                 type='string',
                 callback=lambda option, opt_str, value, parser:
                 ly.warning(_('`--shift-meter` is obsolete and ignored, '
                              'use `--shift-duration` instead')),
                 help=_('ignored; use `--shift-duration` instead'))

    # switch tabstaff clef
    p.add_option('--tc', '--tab-clef',
                 metavar=_("TABCLEFNAME"),
                 action="store",
                 dest="tab_clef",
                 help=_("switch between two versions of tab clefs "
                        '("tab" and "moderntab")'))

    # StringNumber stencil on/off
    p.add_option('--sn', '--string-numbers',
                 metavar=_("t[rue]/f[alse]"),
                 action="store",
                 dest="string_numbers",
                 help=_("deactivate string number stencil with "
                        "--string-numbers f[alse]. Default is t[rue]"))

    # StringNumber stencil on/off
    p.add_option('--fb', '--fretboards',
                 action="store_true",
                 default=False,
                 dest="fretboards",
                 help=_("convert '<frame>' events to a separate "
                        "FretBoards voice instead of markups"))

    p.add_option_group('',
                       description=(
                           _("Report bugs via %s")
                           % 'bug-lilypond@gnu.org') + '\n')
    return p


def print_voice_definitions(printer, part_list, voices):
    for part in part_list:
        part_id = part.id
        nv_dict = voices.get(part_id, {})
        for (name, voice) in list(nv_dict.items()):
            k = music_xml_voice_name_to_lily_name(part_id, name)
            printer.dump('%s =' % k)
            voice.ly_voice.print_ly(printer)
            printer.newline()
            if voice.chordnames:
                cnname = music_xml_chordnames_name_to_lily_name(part_id, name)
                printer.dump('%s =' % cnname)
                voice.chordnames.print_ly(printer)
                printer.newline()
            for l in voice.lyrics_order:
                lname = music_xml_lyrics_name_to_lily_name(part_id, name, l)
                printer.dump('%s =' % lname)
                voice.lyrics_dict[l].print_ly(printer)
                printer.newline()
            if voice.figured_bass:
                fbname = music_xml_figuredbass_name_to_lily_name(part_id, name)
                printer.dump('%s =' % fbname)
                voice.figured_bass.print_ly(printer)
                printer.newline()
            if voice.fretboards:
                fbdname = music_xml_fretboards_name_to_lily_name(part_id, name)
                printer.dump('%s =' % fbdname)
                voice.fretboards.print_ly(printer)
                printer.newline()


# Format staff information to get the following format.
#
# ```
# [staff_id,
#   [
#     [lily_voice_id1,
#       [(lily_lyrics_id11, stanza_id11), (lily_lyrics_id12, stanza_id12) ...],
#       lily_figured_bass_id1,
#       ...],
#     [lily_voice_id2,
#       [(lily_lyrics_id21, stanza_id21), (lily_lyrics_id22, stanza_id22) ...],
#       lily_figured_bass_id2,
#       ...],
#     ...
#   ]
# ]
# ```
#
# The input `raw_voices` is of the form
#
# ```
# [
#   (voice_name_id1,
#    [(lyrics_id11, stanza_id11), (lyrics_id12, stanza_id12), ...]
#    figured_bass_id1,
#    ...)
#   (voice_name_id2,
#    [(lyrics_id21, stanza_id21), (lyrics_id22, stanza_id22), ...]
#    figured_bass_id2,
#    ...)
#   ...
# ]
# ```
def format_staff_info(part_id, staff_id, raw_voices):
    voices = []
    for (v, lyricsids, figured_bass, chordnames, fretboards) in raw_voices:
        voice_name = music_xml_voice_name_to_lily_name(part_id, v)
        voice_lyrics = [
            (music_xml_lyrics_name_to_lily_name(part_id, v, l), stanza_id)
            for (l, stanza_id) in lyricsids]
        figured_bass_name = ''
        if figured_bass:
            figured_bass_name = music_xml_figuredbass_name_to_lily_name(
                part_id, v)
        chordnames_name = ''
        if chordnames:
            chordnames_name = music_xml_chordnames_name_to_lily_name(
                part_id, v)
        fretboards_name = ''
        if fretboards:
            fretboards_name = music_xml_fretboards_name_to_lily_name(
                part_id, v)
        voices.append([voice_name, voice_lyrics, figured_bass_name,
                       chordnames_name, fretboards_name])
    return [staff_id, voices]


def update_score_setup(score_structure, part_list, voices, parts):
    for part_definition in part_list:
        part_id = part_definition.id
        nv_dict = voices.get(part_id)
        if not nv_dict:
            if len(part_list) == len(voices) == 1:
                # If there is only one part, infer the ID.
                # See input/regression/musicxml/41g-PartNoId.xml.
                nv_dict = list(voices.values())[0]
                voices[part_id] = nv_dict
            else:
                ly.warning(_('unknown part in part-list: %s') % part_id)
                continue

        staves = reduce(lambda x, y: x + y,
                        [list(voice.voicedata._staves.keys())
                         for voice in list(nv_dict.values())],
                        [])
        staves_info = []
        if len(staves) > 1:
            staves_info = []
            staves = sorted(set(staves))

            for s in staves:
                thisstaff_raw_voices = []
                for (voice_name, voice) in list(nv_dict.items()):
                    if voice.voicedata._start_staff == s:
                        order = []
                        for i in voice.lyrics_order:
                            order.append((i, voice.lyrics_dict[i].stanza_id))
                        raw_voice = (voice_name,
                                     order,
                                     voice.figured_bass,
                                     voice.chordnames,
                                     voice.fretboards)
                        thisstaff_raw_voices.append(raw_voice)

                staves_info.append(format_staff_info(
                    part_id, s, thisstaff_raw_voices))
        else:
            thisstaff_raw_voices = []
            for (voice_name, voice) in list(nv_dict.items()):
                order = []
                for i in voice.lyrics_order:
                    order.append((i, voice.lyrics_dict[i].stanza_id))
                raw_voice = (voice_name,
                             order,
                             voice.figured_bass,
                             voice.chordnames,
                             voice.fretboards)
                thisstaff_raw_voices.append(raw_voice)

            staves_info.append(format_staff_info(
                part_id, None, thisstaff_raw_voices))
        part = score_structure.find_part(part_id)
        if part is not None:
            part.set_part_information(staves_info)

    sounds = []
    for part in parts:
        for measure in part.get_typed_children(musicxml.Measure):
            sounds.extend(measure.get_typed_children(musicxml.Sound))
            for direction in measure.get_typed_children(musicxml.Direction):
                sounds.extend(direction.get_typed_children(musicxml.Sound))

    score_structure.set_tempo('100')
    if len(sounds) != 0:
        for sound in sounds:
            if sound.get_tempo() is not None and sound.get_tempo() != "":
                score_structure.set_tempo(sound.get_tempo())
                break


# Set global values in the \layout block, like auto-beaming etc.
def update_layout_information():
    if (not conversion_settings.ignore_beaming
            and globvars.layout_information):
        globvars.layout_information.set_context_item(
            'Score', 'autoBeaming = ##f')
    if musicexp.get_string_numbers() == "f":
        globvars.layout_information.set_context_item(
            'Score', r"\override StringNumber #'stencil = ##f")

#  \n\t\t\t\t\\override StringNumber #\'stencil = ##f


def print_ly_preamble(printer, filename):
    printer.dump_version(lilypond_version)
    printer.print_verbatim(
        '% automatically converted by musicxml2ly from ' + filename)
    printer.newline()
    printer.dump(r'\pointAndClickOff')
    printer.newline()
    if options.midi:
        printer.newline()
        printer.dump(r'\include "articulate.ly"')
        printer.newline()
    printer.newline()


def print_ly_additional_definitions(printer):
    if needed_additional_definitions:
        printer.print_verbatim(
            '%% additional definitions required by the score:')
        printer.newline()
    for a in sorted(set(needed_additional_definitions)):
        printer.print_verbatim(definitions.additional_definitions.get(a, ''))
        printer.newline()
    if needed_additional_definitions:
        printer.newline()


def print_ly_additional_macros(printer):
    if additional_macros:
        printer.print_verbatim(
            '%% additional macros required by the score:')
        printer.newline()
    for a in sorted(additional_macros):
        printer.print_verbatim(additional_macros[a])
        printer.newline()
    if additional_macros:
        printer.newline()


# Read in the tree from the given I/O object (file name, file, or bytes) and
# demarshall it using the classes from the musicxml.py file
def read_xml(io_object):
    if isinstance(io_object, bytes):
        io_object = io.BytesIO(io_object)
    from xml.dom import minidom, Node
    doc = minidom.parse(io_object)
    node = doc.documentElement
    return musicxml.minidom_demarshal_node(node)


def read_musicxml(filename, compressed):
    raw_bytes = None
    if compressed:
        if filename == "-":
            ly.progress(
                _("Input is compressed, "
                  "extracting raw MusicXML data from stdin"),
                True)
            # unfortunately, zipfile.ZipFile can't read directly from
            # stdin, so copy everything from stdin to a temp file and read
            # that. TemporaryFile() will remove the file when it is closed.
            tmp = tempfile.TemporaryFile()
            # Make sys.stdin binary
            sys.stdin = os.fdopen(sys.stdin.fileno(), 'rb', 0)
            bytes_read = sys.stdin.read(8192)
            while bytes_read:
                tmp.write(bytes_read)
                bytes_read = sys.stdin.read(8192)
            z = zipfile.ZipFile(tmp, "r")
        else:
            ly.progress(
                _("Input file %s is compressed, "
                  "extracting raw MusicXML data") % filename,
                True)
            z = zipfile.ZipFile(filename, "r")
        container_xml = z.read("META-INF/container.xml")
        if not container_xml:
            return None
        container = read_xml(container_xml)
        if not container:
            return None
        rootfiles = container.get_maybe_exist_named_child('rootfiles')
        if not rootfiles:
            return None
        rootfile_list = rootfiles.get_named_children('rootfile')
        mxml_file = None
        if len(rootfile_list) > 0:
            mxml_file = getattr(rootfile_list[0], 'full-path', None)
        if mxml_file:
            raw_bytes = z.read(mxml_file)

    if raw_bytes is not None:
        io_object = raw_bytes
    elif filename == "-":
        io_object = sys.stdin
    else:
        io_object = filename

    return read_xml(io_object)


def convert(filename, options):
    if filename == "-":
        ly.progress(_("Reading MusicXML from Standard input ..."), True)
    else:
        ly.progress(_("Reading MusicXML from %s ...") % filename, True)

    tree = read_musicxml(filename, options.compressed)
    score_information = extract_score_information(tree)
    paper_information = extract_paper_information(tree)

    parts = tree.get_typed_children(musicxml.Part)
    (voices, staff_info) = get_all_voices(parts)

    score = None
    mxl_pl = tree.get_maybe_exist_typed_child(musicxml.Part_list)
    if mxl_pl:
        score = extract_score_structure(mxl_pl, staff_info)
        part_list = mxl_pl.get_named_children("score-part")

    # Score information is contained in the <work>, <identification> or
    # <movement-title> tags.
    update_score_setup(score, part_list, voices, parts)
    # After the conversion, update the list of settings for the \layout block
    update_layout_information()

    if not options.output_name:
        options.output_name = os.path.basename(filename)
        options.output_name = os.path.splitext(options.output_name)[0]
    elif re.match(r".*\.ly", options.output_name):
        options.output_name = os.path.splitext(options.output_name)[0]

    # defs_ly_name = options.output_name + '-defs.ly'
    if options.output_name == "-":
        output_ly_name = 'Standard output'
    else:
        output_ly_name = options.output_name + '.ly'
    ly.progress(_("Output to `%s'") % output_ly_name, True)
    printer = musicexp.Output_printer()
    # ly.progress(_("Output to `%s'") % defs_ly_name, True)
    if options.output_name == "-":
        printer.set_file(sys.stdout)
    else:
        printer.set_file(open(output_ly_name, 'w', encoding='utf-8'))
    print_ly_preamble(printer, filename)
    print_ly_additional_definitions(printer)
    print_ly_additional_macros(printer)
    if score_information:
        score_information.print_ly(printer)
    if paper_information and conversion_settings.convert_page_layout:
        paper_information.print_ly(printer)
    if globvars.layout_information:
        globvars.layout_information.print_ly(printer)
    print_voice_definitions(printer, part_list, voices)

    printer.newline()
    printer.dump("% The score definition")
    printer.newline()
    score.print_ly(printer)
    printer.newline()

    return voices


def get_existing_filename_with_extension(filename, ext):
    if os.path.exists(filename):
        return filename
    newfilename = filename + "." + ext
    if os.path.exists(newfilename):
        return newfilename
    newfilename = filename + ext
    if os.path.exists(newfilename):
        return newfilename
    return ''


def main():
    opt_parser = option_parser()

    global options
    (options, args) = opt_parser.parse_args()

#   in case of shell entry w/o special characters
    if options.language == 'catalan' or options.language == 'catala':
        options.language = 'català'
    if options.language == 'espanol':
        options.language = 'español'
    if options.language == 'francais':
        options.language = 'français'
    if options.language == 'portugues':
        options.language = 'português'

    if not args:
        opt_parser.print_usage()
        sys.exit(2)

    # midi-block option
    if options.midi:
        musicexp.set_create_midi(options.midi)

    # transpose function
    if options.transpose:
        musicexp.set_transpose(options.transpose)

    # duration shift function
    if options.shift_durations:
        musicexp.set_shift_durations(options.shift_durations)

    # tab clef option
    if options.tab_clef:
        musicexp.set_tab_clef(options.tab_clef)

    # string numbers option
    if options.string_numbers:
        musicexp.set_string_numbers(options.string_numbers)

    if options.absolute_font_sizes:
        musicexp.use_absolute_font_sizes(options.absolute_font_sizes)

    if options.language:
        musicexp.set_pitch_language(options.language)
        needed_additional_definitions.append(options.language)
        definitions.additional_definitions[options.language] = \
            '\\language "%s"\n' % options.language

    conversion_settings.ignore_beaming = not options.convert_beaming
    conversion_settings.convert_page_layout = options.convert_page_layout
    if conversion_settings.convert_page_layout:
        conversion_settings.convert_system_breaks = options.convert_system_breaks
        conversion_settings.convert_page_breaks = options.convert_page_breaks
        conversion_settings.convert_page_margins = options.convert_page_margins
    else:
        conversion_settings.convert_system_breaks = False
        conversion_settings.convert_page_breaks = False
        conversion_settings.convert_page_margins = False
    conversion_settings.convert_stem_directions = options.convert_stem_directions
    conversion_settings.convert_rest_positions = options.convert_rest_positions

    # Allow the user to leave out the .xml or xml on the filename
    basefilename = args[0]
    if basefilename == "-":  # Read from stdin
        filename = "-"
    else:
        filename = get_existing_filename_with_extension(basefilename, "xml")
        if not filename:
            filename = get_existing_filename_with_extension(
                basefilename, "mxl")
            options.compressed = True
    if filename and filename.endswith("mxl"):
        options.compressed = True

    if filename and (filename == "-" or os.path.exists(filename)):
        voices = convert(filename, options)
    else:
        ly.error(_("Unable to find input file %s") % basefilename)
        sys.exit(1)


if __name__ == '__main__':
    main()
