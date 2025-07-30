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
# of version 4.0) to output files containing LilyPond source code.  This is
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
    early_return_val = None
    if not options.tagline:
        # We need a `\paper` block for suppressing the tagline.
        early_return_val = globvars.paper

    defaults = score_partwise.get_maybe_exist_named_child('defaults')
    if not defaults:
        return early_return_val

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
        return early_return_val

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


# Contrary to other parts of MusicXML, "for a series of `<direction-type>`
# children, non-positional formatting attributes are carried over from
# previous elements by default."  The same holds for children of `<credit>`.
#
# Unfortunately, it is not defined what 'non-positional formatting
# attributes' actually means.  The following set of attributes to be ignored
# for this 'carry-over' is thus a heuristic guess, combined with attributes
# `musicxml2ly` doesn't handle.
formatting_attributes_to_ignore = {
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


# A list of MusicXML enclosure types for which `musicxml2ly` provides
# additional support.  Enclosure types unsupported by LilyPond are filtered
# out in function `text_to_ly`.
extra_enclosures = (
    'square'
)


# A map from `<credit-type>` standard values to LilyPond's `\header` fields.
credit_type_dict = {
    '': None,
    'arranger': 'arranger',
    'composer': 'composer',
    'lyricist': 'poet',
    'page number': None,  # Ignored.
    'part name': 'instrument',  # Not ideal because it persists for the
                                # whole document instead of a single page.
    'rights': 'copyright',
    'subtitle': 'subtitle',
    'title': 'title',
}


# Score information is contained in the `<work>`, `<identification>`,
# `<movement-title>`, and `<credit>` elements.  Extract those into a hash
# and map them to LilyPond `\header` fields.
#
# We only handle `<credit>` elements for the 'credit page' as given by the
# command line option `--credit-page`, ignoring the remaining ones.
def extract_score_information(tree):
    header = musicexp.Header()

    def set_if_exists(field, value, alt_field=None, is_markup=False):
        assert field is not None or alt_field is not None

        if value:
            if field == 'texidoc':
                # Don't surround the string with doublequotes yet so that it
                # gets split into words.  Doublequotes are added later in
                # function `dump_texidoc`.
                header.set_field(field, value.replace('"', r'\"'))
            else:
                if is_markup:
                    value = r'\markup \normal-text \normalsize ' + value
                else:
                    value = utilities.escape_ly_output_string(value)

                nonlocal credit_dict

                if field:
                    if alt_field is not None and credit_dict:
                        header.set_field(
                            utilities.escape_ly_output_string(alt_field),
                            value)
                    else:
                        header.set_field(
                            utilities.escape_ly_output_string(field), value)
                else:
                    if credit_dict:
                        header.set_field(
                            utilities.escape_ly_output_string(alt_field),
                            value)

    # If we have one or more `<credit>` elements, don't use metadata for
    # typesetting headers.

    credits = tree.get_named_children('credit')
    credit_page_string = str(options.credit_page)
    credits = [c for c in credits
               if getattr(c, 'page', '1') == credit_page_string]

    credit_group = musicxml.Credit_group(credits)

    # Collect all `<credit>` entries, with and without a type.
    credit_named_dict = {}
    credit_guessed_dict = {}
    no_type_count = 1
    for cred in credits:
        cred_type = cred.get_type()
        if cred_type:
            header_type = credit_type_dict.get(cred_type)
            if header_type is not None:
                credit_named_dict[header_type] = cred
            else:
                # While LilyPond can't process unknown header fields without
                # manual support (i.e., code supplied by the user), it still
                # makes sense to output them.
                cred_type = 'credit: ' + cred_type
                credit_named_dict[cred_type] = cred
        else:
            guessed_type = cred.find_type(credit_group)
            header_type = credit_type_dict.get(guessed_type)
            if header_type is not None:
                credit_guessed_dict[header_type] = cred
            else:
                # If we can't guess a type for an entry, output the data
                # with a sequence number in the field name.
                credit_guessed_dict['credit: ' + str(no_type_count)] = cred
                no_type_count += 1

    # Only use guessed entries for entries that don't have a type.
    credit_dict = dict(list(credit_guessed_dict.items())
                       + list(credit_named_dict.items()))

    for (type, cred) in credit_dict.items():
        elements = []
        attributes = {}

        credit_children = [c for c in cred.get_all_children()
                           if isinstance(c, (musicxml.Credit_words,
                                             musicxml.Credit_symbol))]
        for elem in credit_children:
            # Attributes are 'carried over', so update attributes with data
            # from the current element.
            for a in elem._attribute_dict:
                if a not in formatting_attributes_to_ignore:
                    attributes[a] = elem._attribute_dict[a]

            enclosure = attributes.get('enclosure', None)
            if enclosure is not None and enclosure in extra_enclosures:
                needed_additional_definitions.append(enclosure)

            elements.append((elem, attributes.copy()))

        set_if_exists(type, musicexp.text_to_ly(elements), is_markup=True)

    # Emit metadata.

    work = tree.get_maybe_exist_named_child('work')
    work_title_text = ''
    if work:
        set_if_exists('opus', work.get_work_number(), 'work-number')

        work_title_text = work.get_work_title()
        set_if_exists('title', work_title_text, 'work-title')

        # TODO: Support inclusion of other MusicXML files via the `<opus>`
        # element; see
        #
        #   https://www.w3.org/2021/06/musicxml40/opus-reference/
        #
        # for details.

    movement_title = tree.get_maybe_exist_named_child('movement-title')
    movement_title_text = ''
    if movement_title:
        movement_title_text = movement_title.get_text()

    if movement_title_text:
        if work_title_text:
            field = 'subtitle'
        else:
            field = 'title'
        set_if_exists(field, movement_title_text, 'movement-title')

    movement_number = tree.get_maybe_exist_named_child('movement-number')
    if movement_number:
        # TODO The movement number should be visible in the score,
        #      probably in the 'piece' field of `\header`.
        set_if_exists(None, movement_number.get_text(), 'movement-number')

    identifications = tree.get_named_children('identification')
    for ids in identifications:
        # <rights>
        set_if_exists('copyright', ids.get_rights(), 'id: copyright')

        # <creator>
        set_if_exists('composer', ids.get_composer(), 'id: composer')
        set_if_exists('arranger', ids.get_arranger(), 'id: arranger')
        set_if_exists('editor', ids.get_editor(), 'id: editor')
        set_if_exists('poet', ids.get_poet(), 'id: lyricist')

        # <encoding>
        # We only get the data from the first child, irrespective of its
        # 'type' attribute.
        set_if_exists('id: software',
                      ids.get_encoding_software())  # <software>
        set_if_exists('id: encoding-date',
                      ids.get_encoding_date())  # <encoding-date>
        set_if_exists('id: encoder',
                      ids.get_encoding_person())  # <encoder>
        set_if_exists('id: encoding-description',
                      ids.get_encoding_description())  # <encoding-description>

        # <source>
        set_if_exists('id: source', ids.get_source())

        # <miscellaneous>
        # The element `<miscellaneous-field name="description">` becomes the
        # `texidoc` field in `\header`.
        set_if_exists('texidoc', ids.get_file_description())

        # TODO: Handle `<relation>` element.

        # Finally, apply the required compatibility modes.
        #
        # Some applications created invalid MusicXML files, so we need to
        # apply some compatibility settings, e.g., ignoring some features or
        # elements in such files.
        software = ids.get_encoding_software_list()

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
            # "Sibelius 5.1" with the "Dolet 3.4 for Sibelius" plugin is
            # missing all beam ends; we thus ignore all beaming information.
            app_description = ignore_beaming_software.get(s, False)
            if app_description:
                conversion_settings.ignore_beaming = True
                ly.warning(_("Encountered file created by %s, containing "
                             "wrong beaming information. All beaming "
                             "information in the MusicXML file will be "
                             "ignored") % app_description)

            # Finale encodes ottava ends differently than many other
            # applications.
            if 'Finale' in s:
                musicexp.set_ottavas_end_early('t')

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

    (_, attributes) = list(mxl_attr.items())[0]

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

    # We support `<part-symbol>` only at the beginning of a part.
    part_symbol = attributes.get_maybe_exist_named_child('part-symbol')
    if part_symbol:
        staff.part_symbol = part_symbol.get_text()
        staff.barline_top = int(getattr(part_symbol, 'top-staff', 0))
        staff.barline_bottom = int(getattr(part_symbol, 'bottom-staff', 0))

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

        instrument_name_text = ''

        partname = el.get_maybe_exist_named_child('part-name')
        # Finale gives unnamed parts the name "MusicXML Part" automatically!
        if partname and partname.get_text() != "MusicXML Part":
            instrument_name_text = partname.get_text()
            staff.instrument_name = \
                musicexp.text_to_ly([(partname, partname._attribute_dict)])
        # `<part-name-display>` overrides `<part-name>`.
        partname = el.get_maybe_exist_named_child("part-name-display")
        if partname:
            if getattr(partname, 'print-object', 'yes') == 'yes':
                instrument_name_text = extract_display_text(partname)
                staff.instrument_name = extract_display_markup(partname)
            else:
                staff.instrument_name = None

        if options.midi:
            staff.sound = extract_instrument_sound(el)

        # TODO: Replace this very rough estimate with the first per-system
        #       left margin value:
        #
        #         <print> → <system-layout> → <left-margin>
        if instrument_name_text:
            globvars.paper.indent = max(globvars.paper.indent,
                                        len(instrument_name_text))
            globvars.paper.instrument_names.append(instrument_name_text)

        short_instrument_name_text = ''

        partshort = el.get_maybe_exist_named_child('part-abbreviation')
        if partshort:
            short_instrument_name_text = partshort.get_text()
            staff.short_instrument_name = \
                musicexp.text_to_ly([(partshort, partshort._attribute_dict)])
        # `<part-abbreviation-display>` overrides `<part-abbreviation>`
        partshort = el.get_maybe_exist_named_child('part-abbreviation-display')
        if partshort:
            if getattr(partshort, 'print-object', 'yes') == 'yes':
                short_instrument_name_text = extract_display_text(partshort)
                staff.short_instrument_name = extract_display_markup(partshort)
            else:
                staff.short_instrument_name = None

        # TODO: Read in the MIDI device / instrument

        # TODO: Replace this very rough estimate with the global left margin
        #       value:
        #
        #         <defaults> → <system-layout> → <left-margin>
        if short_instrument_name_text:
            globvars.paper.short_indent = max(globvars.paper.short_indent,
                                              len(short_instrument_name_text))

        return staff

    def read_score_group(el):
        if not isinstance(el, musicxml.Part_group):
            return
        group = musicexp.StaffGroup()
        group_id = getattr(el, 'number', None)
        if group_id is not None:
            group.id = group_id

        groupname = el.get_maybe_exist_named_child('group-name')
        if groupname:
            group.instrument_name = \
                musicexp.text_to_ly([(groupname, groupname._attribute_dict)])
        # `<group-name-display>` overrides `<group-name>`.
        groupname = el.get_maybe_exist_named_child('group-name-display')
        if groupname:
            if getattr(groupname, 'print-object', 'yes') == 'yes':
                group.instrument_name = extract_display_markup(groupname)
            else:
                group.instrument_name = None

        groupshort = el.get_maybe_exist_named_child('group-abbreviation')
        if groupshort:
            group.short_instrument_name = \
                musicexp.text_to_ly([(groupshort, groupshort._attribute_dict)])
        # `<group-abbreviation-display>` overrides `<group-abbreviation>`.
        groupshort = el.get_maybe_exist_named_child(
            'group-abbreviation-display')
        if groupshort:
            if getattr(groupshort, 'print-object', 'yes') == 'yes':
                group.short_instrument_name = \
                    extract_display_markup(groupshort)
            else:
                group.short_instrument_name = None

        groupsymbol = el.get_maybe_exist_named_child('group-symbol')
        if groupsymbol:
            group.symbol = groupsymbol.get_text()

        groupbarline = el.get_maybe_exist_named_child('group-barline')
        if groupbarline:
            group.spanbar = groupbarline.get_text()
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
                ends = el.end.keys()
                prev_started = staves[prev_start].start.keys()
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


# Detect repeats and alternative endings and convert them to corresponding
# 'musicexp' objects, containing nested music.
def group_repeats(music_list):
    def clamp_range(start, stop):
        if start < 0:
            start = 0
        if stop > len(music_list):
            stop = len(music_list)
        return (start, stop)

    # Wrap elements in `music_list` between `markers[start]` and
    # `markers[end]`.
    def wrap_repeat(start, stop):
        nonlocal music_list_pos

        globvars.layout_information.set_context_item(
            'Score', 'doubleRepeatBarType = ":|.|:"')

        start_marker = music_list[start]
        if isinstance(start_marker, conversion.RepeatMarker):
            if start_marker.at_start:
                # We need special LilyPond support for this case.
                globvars.layout_information.set_context_item(
                    'Score', 'printInitialRepeatBar = ##t')

        times = 2
        if stop < len(music_list):
            stop_marker = music_list[stop]
            if type(stop_marker) == conversion.RepeatMarker:
                times = stop_marker.times
                if times == 1:
                    # We need special LilyPond support for this case.
                    globvars.layout_information.set_context_item(
                        'Score', 'printTrivialVoltaRepeats = ##t')

        r = musicexp.RepeatedMusic()
        r.repeat_count = times
        r.set_music(music_list[start + 1:stop])

        (m, n) = clamp_range(start, stop + 1)
        del music_list[m:n]

        music_list.insert(m, r)

        # We have to adjust `music_list_pos` if we modify the beginning of
        # `music_list`.
        if markers[0][1] <= music_list_pos:
            music_list_pos = markers[0][1] + 1

    # Construct a `\repeat` block with alternatives as specified by `start` and
    # `endings`.
    def wrap_repeat_with_endings(start, endings):
        nonlocal music_list_pos

        globvars.layout_information.set_context_item(
            'Score', 'doubleRepeatBarType = ":|.|:"')

        stop = endings[0][0]
        r = musicexp.RepeatedMusic()
        r.set_music(music_list[start + 1:stop])

        repeat_count = 0
        for (i, j) in endings:
            # We have to prepend the data from `<ending>` start elements to set
            # properties for the `VoltaSpanner` grobs.  Note that MusicXML
            # doesn't provide a separate `color` attribute for the volta number
            # text.
            el_start = music_list[i].mxl_event
            attributes = el_start._attribute_dict.copy()
            attributes.pop('color', None)

            v = musicexp.VoltaStyleEvent()
            v.element = (el_start, attributes)

            # Check the `number` attribute of the start and stop `<ending>`
            # elements.
            volte_start = music_list[i].volte;
            volte_stop = music_list[j].volte;
            if volte_start:
                volte = volte_start
            elif volte_stop:
                volte = volte_stop
            else:
                # Handle an empty `volte` list (i.e., MusicXML tells us that
                # the volta number is undetermined, whatever this means).
                # Since LilyPond doesn't print a right-open volta bracket if
                # both the first and the second ending have the same volta
                # number, we use values '1' and '2'.
                el_stop = music_list[j].mxl_event
                if getattr(el_stop, 'type', None) == 'discontinue':
                    volte = [2]
                else:
                    volte = [1]

            repeat_count += len(volte)

            if getattr(el_start, 'print-object', 'yes') == 'no':
                v.visible = False
            v.color = getattr(el_start, 'color', None)

            s = musicexp.SequentialMusic()
            s.elements.append(v)
            s.elements.extend(music_list[i + 1:j])

            r.add_ending(volte, s)

        r.repeat_count = repeat_count

        # Deleting elements from `music_list` in a loop works if we do it in
        # reverse order.  We also delete the repeat markers around the actual
        # data.
        for (i, j) in reversed(endings):
            (m, n) = clamp_range(i, j + 1)
            del music_list[m:n]

        # There is no repeat marker after the element at position `stop`.
        (m, n) = clamp_range(start, stop)
        del music_list[m:n]

        music_list.insert(m, r)

        # We have to adjust `music_list_pos` if we modify the beginning of
        # `music_list`.
        if markers[0][1] <= music_list_pos:
            music_list_pos = markers[0][1] + 1

    marker_dict = {
        (conversion.RepeatEndingMarker, -1): 'REPEAT FORWARD & ENDING START',
        (conversion.RepeatEndingMarker, 1): 'ENDING STOP & REPEAT BACKWARD',
        (conversion.RepeatMarker, -1): 'REPEAT FORWARD',
        (conversion.RepeatMarker, 1): 'REPEAT BACKWARD',
        (conversion.EndingMarker, -1): 'ENDING START',
        (conversion.EndingMarker, 1): 'ENDING STOP',
    }

    marker_id_dict = {
        'REPEAT FORWARD & ENDING START':
            'combination <repeat type="forward"> with <ending type="start">',
        'ENDING STOP & REPEAT BACKWARD':
            'combination <ending type="stop"> with <repeat type="backward">',
        'REPEAT FORWARD': 'element <repeat type="forward">',
        'REPEAT BACKWARD': 'element <repeat type="backward">',
        'ENDING START': 'element <ending type="start">',
        'ENDING STOP': 'element <ending type="stop">',
        '$': 'end of input',
    }

    # Marker objects from `music_list` are stored in the `markers` array.  We
    # only access `music_list` if `markers` is exhausted.
    def get_marker():
        nonlocal markers_pos, music_list_pos

        if markers_pos < len(markers):
            ret = markers[markers_pos][0]
            markers_pos += 1
            return ret

        music_list_end = len(music_list)

        while music_list_pos < music_list_end:
            el = music_list[music_list_pos]
            direction = getattr(el, 'direction', None)
            marker = marker_dict.get((type(el), direction), None)

            if marker is not None:
                markers.append([marker, music_list_pos])
                music_list_pos += 1
                markers_pos += 1
                return marker

            music_list_pos += 1

        return '$'  # end of input

    # Parse, rearrange, and transform the repeat structure data list.  The
    # function returns 'True' if the list should be scanned again and 'False' if
    # the list should be regenerated or if we are done.
    def parse_markers():
        nonlocal music_list_pos

        state = '^'  # begin of input

        while True:
            # If we return False there is no need to adjust the `markers` array
            # (except for what we need for wrapping) since it gets regenerated
            # in the next call of the loop.

            marker = get_marker()
            marker_id = marker_id_dict.get(marker, None)

            curr = markers_pos - 1
            prev = markers_pos - 2

            if state == '^':
                if (marker == 'ENDING START'
                        or marker == 'REPEAT BACKWARD'):
                    # We have an implicit repeat start.
                    markers.insert(0, ['REPEAT FORWARD', -1])
                    return True

                if (marker == 'ENDING STOP'
                        or marker == 'ENDING STOP & REPEAT BACKWARD'
                        or marker == 'REPEAT FORWARD & ENDING START'):
                    ly.warning(_('ignoring unexpected %s') % marker_id)
                    del music_list[markers[curr][1]]
                    return False

                if marker == '$':
                    return False

            # structure: ['ENDING START', pos]
            elif state == 'ENDING START':
                if marker == 'ENDING STOP':
                    start = markers[prev][1]
                    stop = markers[curr][1]
                    markers[prev] = ['last ending', [start, stop]]
                    del markers[curr]
                    return True

                if marker == '$':
                    # We have an implicit ending stop.
                    start = markers[prev][1]
                    markers[prev] = ['last ending', [start, len(music_list)]]
                    return True

                if marker == 'ENDING STOP & REPEAT BACKWARD':
                    start = markers[prev][1]
                    stop = markers[curr][1]
                    markers[prev] = ['endings with repeat', [[start, stop]]]
                    del markers[curr]
                    return True

                if marker == 'ENDING START':
                    ly.warning(_('ignoring unexpected %s') % marker_id)
                    del music_list[markers[curr][1]]
                    return False

            # structure: ['REPEAT FORWARD', pos]
            elif state == 'REPEAT FORWARD':
                if marker == 'endings':
                    start = markers[prev][1]
                    endings = markers[curr][1]
                    wrap_repeat_with_endings(start, endings)
                    return False

                if marker == 'REPEAT BACKWARD':
                    start = markers[prev][1]
                    stop = markers[curr][1]
                    wrap_repeat(start, stop)
                    return False

                if marker == '$':
                    # We have an implicit backward repeat.
                    start = markers[prev][1]
                    stop = len(music_list)

                    # Similar to explicit repeats we ignore the final bar
                    # line's type (if set).
                    el = music_list[stop - 1]
                    if (isinstance(el, musicexp.ChordEvent)
                            and not el.elements):
                        # Ignore empty chord at the end of input.
                        el = music_list[stop - 2]
                    if isinstance(el, musicexp.BarLine):
                        el.type = None

                    wrap_repeat(start, stop)
                    return False

            # structure: ['REPEAT FORWARD & ENDING START', pos]
            elif state == 'REPEAT FORWARD & ENDING START':
                if marker == 'ENDING STOP':
                    # TODO: We ignore the ending position of the volta bracket
                    #       and only use the position of the 'repeat backward &
                    #       ending start' element, letting LilyPond handle the
                    #       volta bracket automatically.  Find a way to control
                    #       the length of the final volta bracket.
                    markers[prev][0] = 'repeat forward & nested ending start'
                    del markers[curr]
                    music_list_pos -= 1
                    del music_list[music_list_pos]
                    # Continue with parsing; don't reset `music_list_pos` for
                    # this case.
                    return True

                if marker == 'REPEAT BACKWARD':
                    # A nested repeat that starts at the same position as the
                    # volta bracket but ends within the volta bracket.
                    start = markers[prev][1]
                    stop = markers[curr][1]

                    el = music_list[start]
                    new_el = conversion.EndingMarker()
                    new_el.direction = el.direction
                    new_el.mxl_event = el.mxl_event
                    new_el.volte = el.volte
                    wrap_repeat(start, stop)
                    music_list.insert(start, new_el)

                    return False

                if marker == '$':
                    # We have an implicit ending and backward repeat.
                    start = markers[prev][1]

                    el = music_list[start]
                    new_el = conversion.EndingMarker()
                    new_el.direction = el.direction
                    new_el.mxl_event = el.mxl_event
                    new_el.volte = el.volte
                    wrap_repeat(start, len(music_list))
                    music_list.insert(start, new_el)

                    return False

                if (marker == 'REPEAT FORWARD & ENDING START'
                        or marker == 'ENDING START'):
                    ly.warning(_('ignoring unexpected %s') % marker_id)
                    del music_list[markers[curr][1]]
                    return False

            # structure: ['repeat forward & nested ending start', pos]
            elif state == 'repeat forward & nested ending start':
                if marker == 'REPEAT BACKWARD':
                    start = markers[prev][1]
                    stop = markers[curr][1]

                    # Emit the nested repeat and put it into a last ending.
                    new_after = conversion.EndingMarker()
                    new_after.direction = 1

                    new_before = conversion.EndingMarker()
                    new_before.direction = -1
                    new_before.mxl_event = music_list[start].mxl_event
                    new_before.volte = music_list[start].volte

                    wrap_repeat(start, stop)
                    music_list.insert(start + 1, new_after)
                    music_list.insert(start, new_before)

                    # Adjust `music_list_pos` for proper re-parsing.
                    if markers[0][1] < music_list_pos:
                        music_list_pos = markers[0][1]
                    return False

            # structure: ['endings with repeat',
            #             [[ending1_start, ending1_stop],
            #              [ending2_start, ending2_stop],
            #               ...]]
            elif state == 'endings with repeat':
                if marker == 'endings with repeat':
                    markers[prev][1].extend(markers[curr][1])
                    del markers[curr]
                    return True

                if marker == 'last ending':
                    markers[prev][0] = 'endings'
                    markers[prev][1].append(markers[curr][1])
                    del markers[curr]
                    return True

            elif state == 'last ending':
                # This catches the unusual situation where a prima volta
                # bracket is ended before the repeat bar.
                if marker == 'REPEAT BACKWARD':
                    ending_stop = markers[prev][1][1]
                    repeat = markers[curr][1]

                    ending_stop_el = music_list[ending_stop]
                    repeat_el = music_list[repeat]

                    # We move the volta bracket end to the repeat bar.
                    music_list[repeat] = \
                        conversion.RepeatEndingMarker(repeat_el,
                                                      ending_stop_el)
                    del music_list[ending_stop]

                    # Adjust `music_list_pos` for proper re-parsing.
                    music_list_pos = markers[0][1]
                    return False
                else:
                    ly.warning(_('adding repeat barline to lone %s')
                               % marker_id)

                    ending_stop = markers[prev][1][1]
                    ending_stop_el = music_list[ending_stop]

                    music_list[ending_stop] = conversion.RepeatEndingMarker(
                        conversion.RepeatMarker(), ending_stop_el)

                    markers[prev][0] = 'endings with repeat'
                    markers[prev][1] = [markers[prev][1]]
                    return True

            state = marker

        return False

    # Try to identify larger structures that can be wrapped with
    # `RepeatedMusic` or `SequentialMusic` objects.  If we have a hit, do
    # this wrapping and start again.
    #
    # `music_list` is modified in-place.
    music_list_pos = 0
    music_list_start = 0

    while True:
        # `markers` is modified in-place.
        markers = []
        markers_pos = 0
        while parse_markers():
            markers_pos = 0
        if music_list_pos == len(music_list):
            break

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
    def get_clef_pitch(clef_stafflines):
        clef_middle_line_pitch = {
            # The pitch of the middle line (i.e., line 3) if the clef is
            # positioned on 'line 0'.
            'G': (3, 1),
            'C': (6, 0),
            'F': (2, 0),
            'percussion': (6, 0),
            'PERC': (6, 0),
        }
        if clef_stafflines.type not in clef_middle_line_pitch.keys():
            return None

        p = musicexp.Pitch()

        (step, octave) = clef_middle_line_pitch[clef_stafflines.type]

        if clef_stafflines.position is not None:
            step -= clef_stafflines.position * 2
        else:
            step = 6

        if clef_stafflines.lines is not None:
            step -= 5 - clef_stafflines.lines

        if step < 0:
            p.step = step + 7
            p.octave = octave - 1 + clef_stafflines.octave
        elif step > 6:
            p.step = step - 7
            p.octave = octave + 1 + clef_stafflines.octave
        else:
            p.step = step
            p.octave = octave + clef_stafflines.octave

        return p

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

    ev.pitch = get_clef_pitch(ev)

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
    children = el.get_all_children()
    if children:
        text = []
        for child in children:
            name = child.get_name()
            if name == 'accidental-text':
                text.append('#')  # This is sufficient for a character count.
            else:
                t = child.get_text()
                if t:
                    text.append(t)
        return ' '.join(text)
    else:
        return ''


def extract_display_markup(el):
    children = el.get_all_children()
    if children:
        elements = []
        for child in children:
            elements.append((child, child._attribute_dict))
        return musicexp.text_to_ly(elements)
    else:
        return None


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
        if getattr(child, 'print-object', 'yes') == 'yes':
            name = extract_display_markup(child)
        else:
            name = ''
        name = musicexp.escape_instrument_string(name)
        elts.append(musicexp.SetEvent('Staff.instrumentName',
                                      '%s' % name))

    child = el.get_maybe_exist_named_child("part-abbreviation-display")
    if child:
        if getattr(child, 'print-object', 'yes') == 'yes':
            name = extract_display_markup(child)
        else:
            name = ''
        name = musicexp.escape_instrument_string(name)
        elts.append(musicexp.SetEvent('Staff.shortInstrumentName',
                                      '%s' % name))

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

    if name == 'slur':
        ev.number = int(attributes.get('number', 1))

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
    # The `note_font_size` argument gets ignored.
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
    # The `note_font_size` argument gets ignored.

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

    # At this point, the attributes for all elements have already been set
    # or derived from previous elements.  We thus can manipulate a single
    # element's attributes without having side effects.
    if options.dynamics_scale is not None:
        if options.dynamics_scale == 0:
            attributes.pop('font-size', None)
        else:
            attributes['font-size-scale'] = options.dynamics_scale

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
        ev.font_size_scale = attributes.get('font-size-scale', 1.0)
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
            ev.font_size_scale = attributes.get('font-size-scale', 1.0)

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
    needed_additional_definitions.append("crescendo")
    return musicxml_dynamics_spanner_to_lily_event(elements, 'cresc')


def musicxml_dim_spanner_to_lily_event(elements):
    needed_additional_definitions.append("decrescendo")
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

    # Also handle children of 'chained' `<direction>` element.
    if n.next is not None:
        for dt in n.next.get_typed_children(musicxml.DirType):
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
                if a not in formatting_attributes_to_ignore:
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
                ((("E", "A", "D", "G", "B") * (lines // 5 + 1))[0:lines])[i])
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
        if (isinstance(e, (musicxml.Text, musicxml.Elision))
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


class LilyPondVoiceBuilder(musicexp.Base):
    def __init__(self):
        self.elements = []
        self.pending_elements = []
        self.pending_last = []
        self.end_moment = 0
        self.begin_moment = 0
        self.ignore_skips = False
        self.has_relevant_elements = False
        self.bar_number = 0
        self.multi_measure_count = 0
        self.multi_measure_rest = None
        self.multi_measure_ev_chord = None  # Containing `multi_measure_rest`.

    def contains(self, elem):
        if self == elem:
            return True
        for e in self.elements:
            if e.contains(elem):
                return True
        return False

    def emit_multi_measure_rest(self, bar_check=True):
        if self.multi_measure_rest:
            # Doing `R1^\markup{...}` would center the markup over the
            # multi-measure rest, which is most certainly not intended.
            # Instead, do `<>^\markup{...} R1`.  Since it doesn't cause a
            # problem, we do this for the remaining pending elements also.
            if self.pending_elements:
                self.add_pending_elements(True)

            self.elements.append(self.multi_measure_ev_chord)
            self.multi_measure_count = 0
            self.multi_measure_rest = None
            self.multi_measure_ev_chord = None

            if bar_check:
                self.add_bar_check()

    def set_duration(self, duration):
        self.end_moment = self.begin_moment + duration

    def current_duration(self):
        return self.end_moment - self.begin_moment

    def add_pending_elements(self, with_empty_chord=False):
        if with_empty_chord:
            self.elements.append(musicexp.EmptyChord())
        self.elements.extend(self.pending_elements)
        self.pending_elements = []

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

    def add_music(self, music, duration, relevant=True, grace=None):
        assert isinstance(music, musicexp.Music)

        self.has_relevant_elements = self.has_relevant_elements or relevant

        is_barline = isinstance(music, musicexp.BarLine)
        self.emit_multi_measure_rest(not is_barline)

        # The elements in `pending_last` were added while processing the
        # previous `<note>` element and must be emitted before the current
        # `<note>` element gets handled.
        if isinstance(music, musicexp.ChordEvent) and self.pending_last:
            self.add_pending_last()

        self.elements.append(music)
        self.begin_moment = self.end_moment
        self.set_duration(duration)

        # Insert all pending dynamics right after the note or rest if it is
        # not a grace note or rest (which we handle separately).
        if isinstance(music, musicexp.ChordEvent) and grace is None:
            if self.pending_elements:
                self.add_pending_elements()

    # Insert some music command that does not affect the position in the
    # measure.
    def add_command(self, command, relevant=True):
        assert isinstance(command, musicexp.Music)

        self.emit_multi_measure_rest()

        self.has_relevant_elements = self.has_relevant_elements or relevant
        self.elements.append(command)

    def add_barline(self, barline, no_bar_number=True):
        # (Re)store `has_relevant_elements` so that a barline alone does not
        # trigger output for figured bass or chord names.
        has_relevant = self.has_relevant_elements

        bar_number = 0 if no_bar_number else self.bar_number

        # Ignore staff and page breaks together with volta-related repeat
        # markers while checking for a bar line right before.
        prev_barline = None
        elem = None
        for elem in reversed(self.elements):
            if not isinstance(elem, (musicexp.Break,
                                     conversion.Marker)):
                break
        if isinstance(elem, musicexp.BarLine):
            prev_barline = elem

        if prev_barline is not None and self.multi_measure_rest is None:
            # If we have an existing bar line object and no pending
            # multi-measure rest, set its bar number.
            prev_barline.bar_number = bar_number
        else:
            # Otherwise add a new bar line object.
            barline.bar_number = bar_number
            self.add_music(barline, 0)

        self.has_relevant_elements = has_relevant

    def add_partial(self, command):
        self.ignore_skips = True
        # insert the partial, but restore relevant_elements (partial is not
        # relevant)
        relevant = self.has_relevant_elements
        self.add_command(command)
        self.has_relevant_elements = relevant

    def add_dynamics(self, dynamic):
        # store the dynamic item(s) until we encounter the next note/rest:
        self.pending_elements.append(dynamic)

    def add_last(self, last):
        # Store items that come right before the next note (or bar line).
        self.pending_last.append(last)

    def add_bar_check(self):
        if self.multi_measure_rest is None:
            b = musicexp.BarLine()
            self.add_barline(b, False)

    def jumpto(self, moment, grace_skip=None):
        current_end = self.end_moment
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
            skip.grace_skip = grace_skip

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
               and not isinstance(self.elements[at], (musicexp.ChordEvent,
                                                      musicexp.BarLine))):
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
    placement = None
    placement_warning = False

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
                # We set the vertical lyrics position based on the
                # `placement` attribute of the very first `<lyric>` element.
                # This is quite inflexible, unfortunately, but LilyPond
                # doesn't have any real support for lyrics positioning that
                # jumps between above and below a staff.
                placement_attr = getattr(lyric, 'placement', 'below')
                if placement is None:
                    placement = placement_attr
                else:
                    if placement != placement_attr and not placement_warning:
                        ly.warning(_('cannot change vertical position of'
                                     ' lyrics within a part'))
                        placement_warning = True

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

    lyrics_dict[lyric_key] = (result, stanza_id, placement)


def musicxml_voice_to_lily_voice(voice, voice_number, starting_grace_skip):
    tremolo_events = []
    tuplet_events = []
    lyrics = {}
    return_value = VoiceData()
    return_value.voicedata = voice

    clef_visible = True
    key_visible = True
    note_visible = True

    # For `<unpitched>` and pitched full-measure rests.
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

    # For slur management.
    slur_count = 0

    # Needed for melismata detection (ignore lyrics on those notes!):
    is_tied = False
    is_chord = False
    is_beamed = False
    ignore_lyrics = False

    # For pedal marks.
    pedal_is_line = False

    current_staff = None

    pending_figured_bass = []
    pending_chordnames = []
    pending_fretboards = []

    is_single_voice = (voice_number == 0)

    voice_builder = LilyPondVoiceBuilder()
    figured_bass_builder = LilyPondVoiceBuilder()
    chordnames_builder = LilyPondVoiceBuilder()
    fretboards_builder = LilyPondVoiceBuilder()

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
    is_senza_misura = False

    for n in voice._elements:
        tie_started = False
        is_note = isinstance(n, musicxml.Note)
        note_grace_skip = None
        skip_grace_skip = None

        staff_change = None
        staff = n.get('staff')
        if staff:
            if ((current_staff is not None and staff != current_staff)
                    or (current_staff is None
                        and staff != voice._start_staff)):
                staff_change = musicexp.StaffChange(staff)
                if isinstance(n, musicxml.Direction):
                    # Check whether we are in 'grace mode'.
                    evc = voice_builder.last_event_chord(n._when)
                    if evc and not evc.elements and evc.grace_elements:
                        evc.append_grace(staff_change)
                        staff_change = None
                if staff_change and not is_note:
                    # A check for `<note>` follows later.
                    voice_builder.add_command(staff_change)
                    staff_change = None
            current_staff = staff

        if isinstance(n, musicxml.Measure):
            try:
                num = int(n.number)
            except ValueError:
                num = 0

            voice_builder.bar_number = num
            figured_bass_builder.bar_number = num
            chordnames_builder.bar_number = num
            fretboards_builder.bar_number = num

            if voice_builder.pending_elements:
                # This handles the corner case of having elements like
                # `<direction>` between `<note>` and `<measure>`, and which
                # already belong to the next bar.  Such elements should
                # actually be aligned at the bar line (at least this is what
                # other programs like Finale or MuseScore do); however, it
                # is not worth the trouble to actually support that.
                voice_builder.add_pending_elements(True)

            if n.senza_misura_length:
                is_senza_misura = True

                # Emission of this element must be delayed until a bar check
                # gets emitted.
                senza_misura_time_signature = musicexp.TimeSignatureChange()
                senza_misura_time_signature.visible = False
                senza_misura_time_signature.fractions = \
                    [n.senza_misura_length.numerator,
                     n.senza_misura_length.denominator]
            else:
                is_senza_misura = False
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
        is_after_grace = (is_note and n.is_after_grace())

        # We have to check whether we must insert a grace skip for
        # synchronization with other staves.
        if starting_grace_skip is not None:
            # Either a note at the beginning of the music ...
            if n._when == 0:
                if is_note:
                    note_grace_skip = starting_grace_skip
                    starting_grace_skip = None
            # ... or the staff starts with a skip.
            else:
                skip_grace_skip = starting_grace_skip
                starting_grace_skip = None

        if not is_chord and not is_after_grace:
            voice_builder.jumpto(n._when, skip_grace_skip)
            figured_bass_builder.jumpto(n._when)
            chordnames_builder.jumpto(n._when)
            fretboards_builder.jumpto(n._when)

        if isinstance(n, musicxml.Barline):
            (barlines, fermatas) = n.to_lily_object()

            if fermatas:
                needed_additional_definitions.append('for-barline')

                voice_builder.add_command(musicexp.ForBarline())
                for f in fermatas:
                    f_ev = musicxml_fermata_to_lily_event(f)
                    voice_builder.add_command(f_ev)

            curr_alterations = alterations.copy()
            for a in barlines:
                if isinstance(a, musicexp.BarLine):
                    voice_builder.add_barline(a)
                    figured_bass_builder.add_barline(a)
                    chordnames_builder.add_barline(a)
                    fretboards_builder.add_barline(a)
                elif isinstance(a, conversion.Marker):
                    voice_builder.add_command(a)
                    figured_bass_builder.add_command(a, False)
                    chordnames_builder.add_command(a, False)
                    fretboards_builder.add_command(a, False)
            continue

        if isinstance(n, musicxml.Print):
            for a in musicxml_print_to_lily(n):
                voice_builder.add_command(a, False)
            continue

        # Print bar checks between measures.  We do this after `jumpto()`
        # calls so that a skip filling up the previous bar (if any) has
        # already been emitted.
        #
        # `_elements[0]` is always a `Measure` element that gets filtered
        # out above.
        if n._measure_position == 0 and n != voice._elements[1]:
            curr_alterations = alterations.copy()

            # When we reach this point in the loop we are at the beginning
            # of a MusicXML measure.  However, we want to emit a bar check
            # *after* the measure, so we start with num == 2.
            num = voice_builder.bar_number
            if num > 1 and num > last_bar_check:
                voice_builder.add_bar_check()
                figured_bass_builder.add_bar_check()
                chordnames_builder.add_bar_check()
                fretboards_builder.add_bar_check()
                last_bar_check = num

        if (n._measure_position == 0
                and n != voice._elements[1]
                and senza_misura_time_signature):
            voice_builder.add_command(senza_misura_time_signature)
            senza_misura_time_signature = None

        if isinstance(n, musicxml.Direction):
            # Check whether `<direction>` has already been converted in
            # another voice, or whether it is chained with another
            # `<direction>` element.
            if n.converted or n.prev is not None:
                continue
            else:
                n.converted = True

                voice_builder.emit_multi_measure_rest()

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
                mm_count = 0

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

                    if current_staff and current_staff != voice._start_staff:
                        # `\change` must be emitted before `\clef`.
                        staff_change = musicexp.StaffChange(voice._start_staff)
                        voice_builder.add_command(staff_change)
                        current_staff = voice._start_staff

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
                    mm_count = a.multiple_rest_length

                    globvars.layout_information.set_context_item(
                        'Score', 'skipBars = ##t')
                    globvars.layout_information.set_context_item(
                        'Staff',
                        r'\override MultiMeasureRest.expand-limit = 1')

                voice_builder.add_command(a)

                if mm_count:
                    # `add_command` might emit a previous multi-measure
                    # rest, which also sets `multi_measure_count` to zero,
                    # so we set this variable after the call.
                    voice_builder.multi_measure_count = mm_count

            continue

        if not n.__class__.__name__ == 'Note':
            n.message(_('unexpected %s; expected %s or %s or %s') %
                      (n, 'Note', 'Attributes', 'Barline'))
            continue

        is_double_note_tremolo = False

        main_event = n.to_lily_object(
            curr_clef,
            conversion_settings.convert_stem_directions,
            conversion_settings.convert_rest_positions)

        if isinstance(main_event, (musicexp.NoteEvent, musicexp.RestEvent)):
            if not main_event.visible:
                note_visible = False
                if main_event.spacing:
                    needed_additional_definitions.append("hide-note")
            else:
                note_visible = True

            # In cross-staff situations it can happen that `n.single_voice`
            # is set while `voice_number` is zero, so we test for the
            # latter, too.
            if voice_number and n.single_voice is not None:
                if is_single_voice != n.single_voice:
                    is_single_voice = n.single_voice

                    vn = 0 if is_single_voice else voice_number
                    voice_selector = musicexp.VoiceSelector(vn)
                    voice_builder.add_command(voice_selector)

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

        # ignore lyrics for notes inside a tie or chord
        ignore_lyrics = is_tied or is_chord

        grace = n.get('grace')
        notations_children = n['notations']
        rest = n.get('rest')
        is_whole_measure_rest = (rest is not None and rest.is_whole_measure())

        if voice_builder.multi_measure_rest is not None:
            if (is_whole_measure_rest
                    and voice_builder.multi_measure_count
                    and grace is None
                    and not notations_children):
                voice_builder.multi_measure_rest.duration.repeat += 1
                voice_builder.multi_measure_count -= 1

                voice_builder.begin_moment = voice_builder.end_moment
                voice_builder.set_duration(n._duration)
                continue
            else:
                voice_builder.emit_multi_measure_rest()

        # At this point we don't have an active multi-measure rest.
        if voice_builder.multi_measure_count:
            if not is_whole_measure_rest:
                ly.warning(_('Not enough rests for multi-measure rest count'))
                voice_builder.multi_measure_count = 0
            else:
                # We ignore the rest's `measure` attribute if we have an
                # explicit multi-measure rest.
                main_event.full_measure_glyph = True
        else:
            full_measure_glyph = getattr(rest, 'measure', None)

            if is_whole_measure_rest:
                voice_builder.multi_measure_count = 1

                if full_measure_glyph != 'no':
                    main_event.full_measure_glyph = True
            elif is_senza_misura and full_measure_glyph == 'yes':
                voice_builder.multi_measure_count = 1
                main_event.full_measure_glyph = True

        if voice_builder.multi_measure_count and main_event.pitch is not None:
            main_event.y_offset = \
                (main_event.pitch.steps() - curr_clef.pitch.steps()) / 2

            # LilyPond uses a different mechanism to control the vertical
            # position of a multi-measure (or full-measure) rest; its
            # (MusicXML) pitch thus must not affect `\relative`.
            main_event.pitch = None

        if main_event and not first_pitch:
            first_pitch = main_event.pitch

        # `ev_chord` starts as an empty `ChordEvent` object that gets filled
        # with items related to the current chord (notes, beams, etc.) while
        # iterating over elements of `voice`.
        ev_chord = voice_builder.last_event_chord(n._when)
        if not ev_chord:
            ev_chord = musicexp.ChordEvent()
            if voice_builder.multi_measure_count:
                voice_builder.multi_measure_ev_chord = ev_chord
                voice_builder.multi_measure_rest = main_event
                voice_builder.begin_moment = voice_builder.end_moment
                voice_builder.set_duration(n._duration)
            else:
                voice_builder.add_music(ev_chord, n._duration, grace=grace)
        else:
            # This catches '<grace note> <dynamics> <main note>'.
            if voice_builder.pending_elements and 'chord' not in n:
                voice_builder.add_pending_elements()

        # A staff change might happen anywhere; for this reason we have more
        # checks here and below.
        if staff_change and 'chord' in n:
            # TODO: Handle cross-staff chords.
            staff_change = None

        if grace is not None or note_grace_skip is not None:
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
                    for pd in voice_builder.pending_elements:
                        ev_chord.append_after_grace(pd)
                    voice_builder.pending_elements = []
            else:
                if ev_chord.grace_elements and is_chord:
                    grace_chord = \
                        ev_chord.grace_elements.get_last_event_chord()
                if not grace_chord:
                    grace_chord = musicexp.ChordEvent()
                    if staff_change:
                        ev_chord.append_grace(staff_change)
                        staff_change = None
                    if note_grace_skip is not None:
                        skip = musicexp.SkipEvent()
                        skip.duration = note_grace_skip
                        ev_chord.append_grace(skip)
                    ev_chord.append_grace(grace_chord)
                    for pd in voice_builder.pending_elements:
                        ev_chord.append_grace(pd)
                    voice_builder.pending_elements = []

            if not is_after_grace and grace is not None:
                if getattr(grace, 'slash', None) == 'yes':
                    ev_chord.grace_type = "slashed"

            if grace is not None:
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

            if voice_builder.multi_measure_count:
                voice_builder.multi_measure_count -= 1
            else:
                # When a note or chord has grace notes (which have no
                # duration), the duration of the event chord is not yet
                # known.  However, the event chord was already added with
                # duration 0, so we have to correct this when we process the
                # main note.
                if voice_builder.current_duration() == 0 and n._duration > 0:
                    voice_builder.set_duration(n._duration)

        # if we have a figured bass, set its voice builder to the correct
        # position and insert the pending figures
        if pending_figured_bass:
            figured_bass_builder.jumpto(n._when)

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
            chordnames_builder.jumpto(n._when)

            for cn in pending_chordnames:
                # Assign the duration of the EventChord
                cn.duration = ev_chord.get_duration()
                chordnames_builder.add_music(cn, ev_chord.get_length())
            pending_chordnames = []

        if pending_fretboards:
            fretboards_builder.jumpto(n._when)

            for fb in pending_fretboards:
                # Assign the duration of the EventChord
                fb.duration = ev_chord.get_duration()
                fretboards_builder.add_music(fb, ev_chord.get_length())
            pending_fretboards = []

        color = getattr(n, 'color', None)
        font_size = getattr(n, 'font-size', None)

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

            endslurs = [s for s in notations['slur']
                        if s.get_type() in ('stop')]
            for es in endslurs:
                if slur_count == 0:
                    es.message(_('Encountered closing slur, '
                                 'but no slur is open'))
                else:
                    slur_count -= 1

                lily_ev = musicxml_spanner_to_lily_event(es)
                ev_chord.append(lily_ev)

            startslurs = [s for s in notations['slur']
                          if s.get_type() in ('start')]
            for ss in startslurs:
                slur_count += 1
                lily_ev = musicxml_spanner_to_lily_event(ss)
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

    # For getting a correct value in the last bar check comment.
    voice_builder.bar_number += 1

    # Force trailing multi-measure rests and/or pending elements to be
    # written out.
    ce = musicexp.ChordEvent()
    if (voice_builder.multi_measure_rest is None
            and voice_builder.pending_elements):
        # We have elements that are positioned after the last `<note>`,
        # i.e., after the music has finished.  To make LilyPond output them
        # we need an 'anchor', so we append a skip with a very short length.
        s = musicexp.SkipEvent()
        s.duration.set_from_fraction(Fraction(1, 1024))
        ce.append(s)
    voice_builder.add_music(ce, 0)

    ly_voice = group_tremolos(voice_builder.elements, tremolo_events)
    ly_voice = group_tuplets(ly_voice, tuplet_events)
    ly_voice = group_repeats(ly_voice)

    seq_music = musicexp.SequentialMusic()

    seq_music.elements = ly_voice
    for k, v in lyrics.items():
        ev = musicexp.Lyrics()
        ev.lyrics_syllables = v[0]
        ev.stanza_id = v[1]
        ev.placement = v[2]
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
    part.tag_single_voices()
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

    if musicxml.starting_grace_lengths:
        musicxml.max_starting_grace_length = \
            max(musicxml.starting_grace_lengths.values())

    return dictionary


def get_all_voices(parts):
    all_voices = voices_in_part_in_parts(parts)

    all_ly_voices = {}
    all_ly_staffinfo = {}
    for p, (name_voice, staff_info) in all_voices.items():
        num_voices = len(name_voice)
        part_ly_voices = OrderedDict()

        if num_voices:
            voices_in_staves_counter = {}
            staves_counter = {}
            for n, v in name_voice.items():
                staff = v._start_staff
                if staff not in staves_counter:
                    staves_counter[staff] = 0
                staves_counter[staff] += 1

        for n, v in name_voice.items():
            ly.progress(_("Converting part '%s' (voice %s) "
                          "to LilyPond expressions...") % (p, n), True)

            starting_grace_length = \
                musicxml.starting_grace_lengths.get((p, n), 0)
            length = musicxml.max_starting_grace_length - starting_grace_length
            if length:
                starting_grace_skip = musicexp.Duration.from_fraction(length)
            else:
                starting_grace_skip = None

            if num_voices > 1:
                # The code to get the voice number in a staff should stay in
                # sync with `update_score_setup`.
                staff = v._start_staff
                if staves_counter[staff] > 1:
                    if staff not in voices_in_staves_counter:
                        voices_in_staves_counter[staff] = 0
                    voices_in_staves_counter[staff] += 1
                    voice_in_staff = voices_in_staves_counter[staff]
                else:
                    voice_in_staff = 0
            else:
                voice_in_staff = 0

            # `musicxml_voice_to_lily_voice` returns a `VoiceData` object.
            voice = musicxml_voice_to_lily_voice(v,
                                                 voice_in_staff,
                                                 starting_grace_skip)
            part_ly_voices[n] = voice

        all_ly_voices[p] = part_ly_voices
        all_ly_staffinfo[p] = staff_info

    return (all_ly_voices, all_ly_staffinfo)


def option_parser():
    p = ly.get_option_parser(usage=_("musicxml2ly [OPTION]... FILE"),
                             description=_("""\
Convert FILE with MusicXML data to a LilyPond input file.

By default, the output file is called 'FILE.ly' for input file 'FILE.xml'.
If FILE is '-', read from standard input (and write to standard output).

If FILE cannot be found, 'FILE.xml', 'FILE.musicxml', and 'FILE.mxl' are
also tried as input files.
"""),
                             add_help_option=False)

    p.add_option("-h", "--help",
                 action="help",
                 help=_("show this help and exit"))

    p.version = ('%prog (LilyPond) ' + lilypond_version + '\n\n'
                 +
                 _("""Copyright (c) 2005--2023 by
    Han-Wen Nienhuys <hanwen@xs4all.nl>,
    Jan Nieuwenhuizen <janneke@gnu.org>,
    Reinhold Kainhofer <reinhold@kainhofer.com>,
    Patrick L. Schmidt <pls@philomelos.net>, and
    Werner Lemberg <wl@gnu.org>
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

    p.add_option('--oe', '--ottavas-end-early',
                 metavar=_('t[rue]/f[alse]'),
                 action='store',
                 dest='ottavas_end_early',
                 help=_('expect <octave-shift> end elements before '
                        'the associated <note> (as in Finale). '
                        'Default is f[alse]'))

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

    p.add_option('--ds', '--dynamics-scale',
                 metavar=_("FACTOR"),
                 action='store',
                 default=None,
                 type='float',
                 dest='dynamics_scale',
                 help=_("scale <dynamics> elements by a non-negative FACTOR; "
                        "value 0 means to use lilypond's standard size for "
                        "dynamics"))

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

    p.add_option('--cp', '--credit-page',
                 metavar=_("N"),
                 action="store",
                 dest="credit_page",
                 default=1,
                 type='int',
                 help=_('use <credit> information from page N '
                        'to fill the fields of the \\header block. '
                        'Default is value 1'))

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

    # string numbers on/off
    p.add_option('--sn', '--string-numbers',
                 metavar=_("t[rue]/f[alse]"),
                 action="store",
                 dest="string_numbers",
                 help=_("control output of string numbers; value f[alse] "
                        "disables them. Default is t[rue]"))

    # separate FretBoards voice
    p.add_option('--fb', '--fretboards',
                 action="store_true",
                 default=False,
                 dest="fretboards",
                 help=_("convert '<frame>' events to a separate "
                        "FretBoards voice instead of markups"))

    p.add_option('--book',
                 action='store_true',
                 default=False,
                 help=_(r"put top-level score into '\book'"))

    p.add_option('--nt', '--no-tagline',
                 action='store_false',
                 default=True,
                 dest='tagline',
                 help=_("don't emit a LilyPond tagline"))

    p.add_option_group('',
                       description=(
                           _("Report bugs via %s")
                           % 'bug-lilypond@gnu.org') + '\n')
    return p


def print_voice_definitions(printer, part_list, voices):
    for part in part_list:
        part_id = part.id
        nv_dict = voices.get(part_id, {})
        for (name, voice) in nv_dict.items():
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
#       [(lily_lyrics_id11, stanza_id11, placement11),
#        (lily_lyrics_id12, stanza_id12, placement12),
#        ...],
#       lily_figured_bass_id1,
#       ...],
#     [lily_voice_id2,
#       [(lily_lyrics_id21, stanza_id21, placement21),
#        (lily_lyrics_id22, stanza_id22, placement22),
#        ...],
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
#    [(lyrics_id11, stanza_id11, placement11),
#     (lyrics_id12, stanza_id12, placement12),
#     ...]
#    figured_bass_id1,
#    ...)
#   (voice_name_id2,
#    [(lyrics_id21, stanza_id21, placement21),
#     (lyrics_id22, stanza_id22, placement22),
#     ...]
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
            (music_xml_lyrics_name_to_lily_name(part_id, v, l),
             stanza_id,
             placement)
            for (l, stanza_id, placement) in lyricsids]
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
                nv_dict = list(voices.values())[0]
                voices[part_id] = nv_dict
            else:
                ly.warning(_('unknown part in part-list: %s') % part_id)
                continue

        staves = reduce(lambda x, y: x + y,
                        [list(voice.voicedata._staves.keys())
                         for voice in nv_dict.values()],
                        [])
        staves_info = []
        if len(staves) > 1:
            staves_info = []
            staves = sorted(set(staves))

            for s in staves:
                thisstaff_raw_voices = []
                for (voice_name, voice) in nv_dict.items():
                    if voice.voicedata._start_staff == s:
                        order = []
                        for i in voice.lyrics_order:
                            order.append((i,
                                          voice.lyrics_dict[i].stanza_id,
                                          voice.lyrics_dict[i].placement))
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
            for (voice_name, voice) in nv_dict.items():
                order = []
                for i in voice.lyrics_order:
                    order.append((i,
                                  voice.lyrics_dict[i].stanza_id,
                                  voice.lyrics_dict[i].placement))
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

    # Mark last (i.e., bottommost) `Staff` element.
    contents = score_structure.contents
    while type(contents) == musicexp.StaffGroup:
        contents = contents.children[-1]
    contents.is_last_staff = True

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
    return ''


def main():
    opt_parser = option_parser()

    global options
    (options, args) = opt_parser.parse_args()

    # Support historical note name aliases similar to LilyPond.
    if options.language == 'catalan':
        options.language = 'català'
    if options.language == 'espanol':
        options.language = 'español'
    if options.language == 'portugues':
        options.language = 'português'

    if not args:
        opt_parser.print_usage()
        sys.exit(2)

    # midi-block option
    if options.midi:
        musicexp.set_create_midi(options.midi)

    # ottavas end early option
    if options.ottavas_end_early:
        musicexp.set_ottavas_end_early(options.ottavas_end_early)

    # transpose function
    if options.transpose:
        musicexp.set_transpose(options.transpose)

    # duration shift function
    if options.shift_durations:
        musicexp.set_shift_durations(options.shift_durations)

    # dynamics scale function
    if options.dynamics_scale is not None:
        if options.dynamics_scale < 0:
            ly.warning(_('requested dynamics scale factor must be '
                         'non-negative, setting to 0'))
            options.dynamics_scale = 0

    # tab clef option
    if options.tab_clef:
        musicexp.set_tab_clef(options.tab_clef)

    # string numbers option
    if options.string_numbers:
        musicexp.set_string_numbers(options.string_numbers)

    # book option
    if options.book:
        musicexp.set_book(options.book)

    # no-tagline option
    if options.tagline:
        musicexp.set_tagline(options.tagline)

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

    # Allow the user to leave out the extension of the filename.
    basefilename = args[0]
    if basefilename == "-":  # Read from stdin
        filename = "-"
    else:
        filename = get_existing_filename_with_extension(basefilename, "xml")
        if not filename:
            filename = get_existing_filename_with_extension(
                basefilename, 'musicxml')
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
