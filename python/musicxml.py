# musicxml.py
# -*- coding: utf-8 -*-
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>,
#               2007--2011 Reinhold Kainhofer <reinhold@kainhofer.com>
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


from collections import OrderedDict
import copy
from fractions import Fraction
import re
import sys
import warnings

import lilylib as ly
import musicexp
import musicxml2ly_conversion
import utilities


class Xml_node(object):

    def __init__(self):
        self._children = []
        self._data = None
        self._original = None
        self._name = 'xml_node'
        self._parent = None
        self._attribute_dict = {}

    def get_parent(self):
        return self._parent

    def is_first(self):
        return self._parent.get_typed_children(self.__class__)[0] == self

    def original(self):
        return self._original

    def get_name(self):
        return self._name

    def get_text(self):
        if self._data:
            return self._data

        if not self._children:
            return ''

        return ''.join([c.get_text() for c in self._children])

    def message(self, msg):
        ly.warning(msg)

        p = self
        while p:
            ly.progress('  In: <%s %s>\n' % (p._name, ' '.join(
                ['%s=%s' % item for item in list(p._attribute_dict.items())])))
            p = p.get_parent()

    def dump(self, indent=''):
        ly.debug_output('%s<%s%s>' % (indent, self._name, ''.join(
            [' %s=%s' % item for item in list(self._attribute_dict.items())])))
        non_text_children = [
            c for c in self._children if not isinstance(c, Hash_text)]
        if non_text_children:
            ly.debug_output('\n')
        for c in self._children:
            c.dump(indent + "    ")
        if non_text_children:
            ly.debug_output(indent)
        ly.debug_output('</%s>\n' % self._name)

    def get_typed_children(self, klass):
        if not klass:
            return []
        else:
            return [c for c in self._children if isinstance(c, klass)]

    def get_named_children(self, nm):
        return self.get_typed_children(get_class(nm))

    def get_named_child(self, nm):
        return self.get_maybe_exist_named_child(nm)

    def get_children(self, predicate):
        return [c for c in self._children if predicate(c)]

    def get_all_children(self):
        return self._children

    def get_maybe_exist_named_child(self, name):
        return self.get_maybe_exist_typed_child(get_class(name))

    def get_maybe_exist_typed_child(self, klass):
        cn = self.get_typed_children(klass)
        if len(cn) == 0:
            return None
        else:
            if len(cn) > 1:
                warnings.warn(_('more than one child of class %s, all but'
                                ' the first will be ignored') % klass.__name__)
            return cn[0]

    def get_unique_typed_child(self, klass):
        cn = self.get_typed_children(klass)
        if len(cn) != 1:
            ly.error(self.__dict__)
            raise RuntimeError(
                'Child is not unique for %s found %d' % (klass, cn))

        return cn[0]

    def get_named_child_value_number(self, name, default):
        n = self.get_maybe_exist_named_child(name)
        if n:
            return int(n.get_text())
        else:
            return default


class Music_xml_node(Xml_node):
    def __init__(self):
        Xml_node.__init__(self)
        self.duration = Fraction(0)
        self.start = Fraction(0)
        self.converted = False
        self.voice_id = None


class Music_xml_spanner(Music_xml_node):

    def get_type(self):
        if hasattr(self, 'type'):
            return self.type
        else:
            return 0

    def get_size(self):
        if hasattr(self, 'size'):
            return int(self.size)
        else:
            return 0


class Measure_element(Music_xml_node):

    def get_voice_id(self):
        voice = self.get_maybe_exist_named_child('voice')
        if voice:
            return voice.get_text()
        else:
            return self.voice_id

    def is_first(self):
        # Look at all measure elements(previously we had self.__class__, which
        # only looked at objects of the same type!
        cn = self._parent.get_typed_children(Measure_element)
        # But only look at the correct voice; But include Attributes, too, which
        # are not tied to any particular voice
        cn = [c for c in cn if(
            c.get_voice_id() == self.get_voice_id()) or isinstance(c, Attributes)]
        return cn[0] == self


class Work(Xml_node):

    def get_work_information(self, tag):
        wt = self.get_maybe_exist_named_child(tag)
        if wt:
            return wt.get_text()
        else:
            return ''

    def get_work_title(self):
        return self.get_work_information('work-title')

    def get_work_number(self):
        return self.get_work_information('work-number')

    # def get_opus(self):
    #     return self.get_work_information('opus')


class Identification(Xml_node):

    def get_rights(self):
        rights = self.get_named_children('rights')
        ret = []
        for r in rights:
            text = r.get_text()
            # if this Xml_node has an attribute, such as 'type="words"',
            # include it in the header. Otherwise, it is assumed that
            # the text contents of this node looks something like this:
            # 'Copyright: X.Y.' and thus already contains the relevant
            # information.
            if hasattr(r, 'type'):
                rights_type = r.type.title()  # capitalize first letter
                result = ''.join([rights_type, ': ', text])
                ret.append(result)
            else:
                ret.append(text)
        return "\n".join(ret)

    # get contents of the source-element(usually used for publishing information).(These contents are saved in a custom variable named "source" in the header of the .ly file.)
    def get_source(self):
        source = self.get_named_children('source')
        ret = []
        for r in source:
            ret.append(r.get_text())
        return "\n".join(ret)

    def get_creator(self, type):
        creators = self.get_named_children('creator')
        # return the first creator tag that has the particular type
        for i in creators:
            if hasattr(i, 'type') and i.type == type:
                return i.get_text()
        return None

    def get_composer(self):
        c = self.get_creator('composer')
        if c:
            return c
        creators = self.get_named_children('creator')
        # return the first creator tag that has no type at all
        for i in creators:
            if not hasattr(i, 'type'):
                return i.get_text()
        return None

    def get_arranger(self):
        return self.get_creator('arranger')

    def get_editor(self):
        return self.get_creator('editor')

    def get_poet(self):
        v = self.get_creator('lyricist')
        if v:
            return v
        v = self.get_creator('poet')
        return v

    def get_encoding_information(self, type):
        enc = self.get_named_children('encoding')
        if enc:
            children = enc[0].get_named_children(type)
            if children:
                return children[0].get_text()
        else:
            return None

    def get_encoding_software(self):
        return self.get_encoding_information('software')

    def get_encoding_date(self):
        return self.get_encoding_information('encoding-date')

    def get_encoding_person(self):
        return self.get_encoding_information('encoder')

    def get_encoding_description(self):
        return self.get_encoding_information('encoding-description')

    def get_encoding_software_list(self):
        enc = self.get_named_children('encoding')
        software = []
        for e in enc:
            softwares = e.get_named_children('software')
            for s in softwares:
                software.append(s.get_text())
        return software

    def get_file_description(self):
        misc = self.get_named_children('miscellaneous')
        for m in misc:
            misc_fields = m.get_named_children('miscellaneous-field')
            for mf in misc_fields:
                if hasattr(mf, 'name') and mf.name == 'description':
                    return mf.get_text()
        return None


class Credit(Xml_node):

    def get_type(self):
        type = self.get_maybe_exist_named_child('credit-type')
        if type is not None:
            return type.get_text()
        else:
            return None

    def find_type(self, credits):
        sizes = self.get_font_sizes(credits)
        sizes.sort(reverse=True)
        ys = self.get_default_ys(credits)
        ys.sort(reverse=True)
        xs = self.get_default_xs(credits)
        xs.sort(reverse=True)

        # Words child of the self credit-element
        words = self.get_maybe_exist_named_child('credit-words')
        size = None
        x = None
        y = None
        halign = None
        valign = None
        justify = None
        if words is not None:
            if hasattr(words, 'font-size'):
                size = int(float((getattr(words, 'font-size'))))
            if hasattr(words, 'default-x'):
                x = round(float(getattr(words, 'default-x')))
            if hasattr(words, 'default-y'):
                y = round(float(getattr(words, 'default-y')))
            if hasattr(words, 'halign'):
                halign = getattr(words, 'halign')
            if hasattr(words, 'valign'):
                valign = getattr(words, 'valign')
            if hasattr(words, 'justify'):
                justify = getattr(words, 'justify')
        if (size and size == max(sizes) and y and y == max(ys) and
              (justify or halign) and (justify == 'center' or halign == 'center')):
            return 'title'
        elif (y and y > min(ys) and y < max(ys) and (justify or halign) and
              (justify == 'center' or halign == 'center')):
            return 'subtitle'
        elif ((justify or halign) and (justify == 'left' or halign == 'left') and
              (not x or x == min(xs))):
            return 'lyricist'
        elif ((justify or halign) and (justify == 'right' or halign == 'right')
              and (not x or x == max(xs))):
            return 'composer'
        elif size and size == min(sizes) and y == min(ys):
            return 'rights'
        # Special cases for Finale NotePad
        elif valign and valign == 'top' and y and y == ys[1]:
            return 'subtitle'
        elif valign and valign == 'top' and x and x == min(xs):
            return 'lyricist'
        elif valign and valign == 'top' and y and y == min(ys):
            return 'rights'
        # Other special cases
        elif valign and valign == 'bottom':
            return 'rights'
        elif len([i for i, item in enumerate(ys) if item == y]) == 2:
            # The first one is the composer, the second one is the lyricist
            return 'composer'

        return None  # no type recognized

    def get_font_sizes(self, credits):
        sizes = []
        for cred in credits:
            words = cred.get_maybe_exist_named_child('credit-words')
            if((words is not None) and hasattr(words, 'font-size')):
                sizes.append(getattr(words, 'font-size'))
        return [int(float(size)) for size in sizes]

    def get_default_xs(self, credits):
        default_xs = []
        for cred in credits:
            words = cred.get_maybe_exist_named_child('credit-words')
            if((words is not None) and hasattr(words, 'default-x')):
                default_xs.append(getattr(words, 'default-x'))
        return list(map(round, list(map(float, default_xs))))

    def get_default_ys(self, credits):
        default_ys = []
        for cred in credits:
            words = cred.get_maybe_exist_named_child('credit-words')
            if words is not None and hasattr(words, 'default-y'):
                default_ys.append(getattr(words, 'default-y'))
        return list(map(round, list(map(float, default_ys))))

    def get_text(self):
        words = self.get_maybe_exist_named_child('credit-words')
        if words is not None:
            return words.get_text()
        else:
            return ''


class Duration(Music_xml_node):

    def get_length(self):
        dur = int(self.get_text()) * Fraction(1, 4)
        return dur


class Hash_text(Music_xml_node):

    def dump(self, indent=''):
        ly.debug_output(self._data.strip())


class Pitch(Music_xml_node):

    def get_step(self):
        ch = self.get_unique_typed_child(get_class('step'))
        step = ch.get_text().strip()
        return step

    def get_octave(self):
        ch = self.get_unique_typed_child(get_class('octave'))
        octave = ch.get_text().strip()
        return int(octave)

    def get_alteration(self):
        ch = self.get_maybe_exist_typed_child(get_class('alter'))
        return utilities.interpret_alter_element(ch)

    def to_lily_object(self):
        p = musicexp.Pitch()
        p.alteration = self.get_alteration()
        p.step = musicxml2ly_conversion.musicxml_step_to_lily(self.get_step())
        p.octave = self.get_octave() - 4
        return p


class Unpitched(Music_xml_node):

    def get_step(self):
        ch = self.get_unique_typed_child(get_class('display-step'))
        step = ch.get_text().strip()
        return step

    def get_octave(self):
        ch = self.get_unique_typed_child(get_class('display-octave'))

        if ch:
            octave = ch.get_text().strip()
            return int(octave)
        else:
            return None

    def to_lily_object(self):
        p = None
        step = self.get_step()
        if step:
            p = musicexp.Pitch()
            p.step = musicxml2ly_conversion.musicxml_step_to_lily(step)
        octave = self.get_octave()
        if octave and p:
            p.octave = octave - 4
        return p


class Measure_element (Music_xml_node):
    def get_voice_id(self):
        voice = self.get_maybe_exist_named_child('voice')
        if voice:
            return voice.get_text()
        else:
            return self.voice_id


class Attributes(Measure_element):

    def __init__(self):
        Measure_element.__init__(self)
        self._dict = {}
        self._original_tag = None
        self._time_signature_cache = None

    def is_first(self):
        cn = self._parent.get_typed_children(self.__class__)
        if self._original_tag:
            return cn[0] == self._original_tag
        else:
            return cn[0] == self

    def set_attributes_from_previous(self, dict):
        self._dict.update(dict)

    def read_self(self):
        for c in self.get_all_children():
            self._dict[c.get_name()] = c

    def get_named_attribute(self, name):
        return self._dict.get(name)

    def single_time_sig_to_fraction(self, sig):
        if len(sig) < 2:
            return 0
        n = 0
        for i in sig[0:-1]:
            n += i
        return Fraction(n, sig[-1])

    def get_measure_length(self):
        sig = self.get_time_signature()
        if not sig or len(sig) == 0:
            return 1
        if isinstance(sig[0], list):
            # Complex compound time signature
            l = 0
            for i in sig:
                l += self.single_time_sig_to_fraction(i)
            return l
        else:
           # Simple(maybe compound) time signature of the form(beat, ..., type)
            return self.single_time_sig_to_fraction(sig)
        return 0

    def get_time_signature(self):
        "Return time sig as a(beat, beat-type) tuple. For compound signatures,"
        "return either(beat, beat,..., beat-type) or((beat,..., type), "
        "(beat,..., type), ...)."
        if self._time_signature_cache:
            return self._time_signature_cache

        try:
            mxl = self.get_named_attribute('time')
            if not mxl:
                return None

            if mxl.get_maybe_exist_named_child('senza-misura'):
                # TODO: Handle pieces without a time signature!
                ly.warning(
                    _("Senza-misura time signatures are not yet supported!"))
                return(4, 4)
            else:
                signature = []
                current_sig = []
                for i in mxl.get_all_children():
                    if isinstance(i, Beats):
                        beats = i.get_text().strip().split("+")
                        current_sig = [int(j) for j in beats]
                    elif isinstance(i, BeatType):
                        current_sig.append(int(i.get_text()))
                        signature.append(current_sig)
                        current_sig = []
                if isinstance(signature[0], list) and len(signature) == 1:
                    signature = signature[0]
                self._time_signature_cache = signature
                return signature
        except(KeyError, ValueError):
            self.message(
                _("Unable to interpret time signature! Falling back to 4/4."))
            return(4, 4)

    # returns clef information in the form("cleftype", position, octave-shift)
    def get_clef_information(self):
        clefinfo = ['G', 2, 0]
        mxl = self.get_named_attribute('clef')
        if not mxl:
            return clefinfo
        sign = mxl.get_maybe_exist_named_child('sign')
        if sign:
            clefinfo[0] = sign.get_text()
        line = mxl.get_maybe_exist_named_child('line')
        if line:
            clefinfo[1] = int(line.get_text())
        octave = mxl.get_maybe_exist_named_child('clef-octave-change')
        if octave:
            clefinfo[2] = int(octave.get_text())
        return clefinfo

    def get_key_signature(self):
        "return(fifths, mode) tuple if the key signatures is given as "
        "major/minor in the Circle of fifths. Otherwise return an alterations"
        "list of the form [[step,alter<,octave>], [step,alter<,octave>], ...], "
        "where the octave values are optional."

        key = self.get_named_attribute('key')
        if not key:
            return None
        fifths_elm = key.get_maybe_exist_named_child('fifths')
        if fifths_elm:
            mode_node = key.get_maybe_exist_named_child('mode')
            mode = None
            if mode_node:
                mode = mode_node.get_text()
            if not mode or mode == '':
                mode = 'major'
            fifths = int(fifths_elm.get_text())
            # TODO: Shall we try to convert the key-octave and the cancel, too?
            return(fifths, mode)
        else:
            alterations = []
            current_step = 0
            for i in key.get_all_children():
                if isinstance(i, KeyStep):
                    current_step = i.get_text().strip()
                elif isinstance(i, KeyAlter):
                    alterations.append(
                        [current_step, utilities.interpret_alter_element(i)])
                elif isinstance(i, KeyOctave):
                    nr = -1
                    if hasattr(i, 'number'):
                        nr = int(i.number)
                    if(nr > 0) and (nr <= len(alterations)):
                        # MusicXML Octave 4 is middle C -> shift to 0
                        alterations[nr - 1].append(int(i.get_text()) - 4)
                    else:
                        i.message(_("Key alteration octave given for a "
                                    "non-existing alteration nr. %s, available numbers: %s!") % (nr, len(alterations)))
            return alterations

    def get_transposition(self):
        return self.get_named_attribute('transpose')


class Barline(Measure_element):

    def to_lily_object(self):
        # retval contains all possible markers in the order:
        # 0..bw_ending, 1..bw_repeat, 2..barline, 3..fw_repeat, 4..fw_ending
        retval = {}
        bartype_element = self.get_maybe_exist_named_child("bar-style")
        repeat_element = self.get_maybe_exist_named_child("repeat")
        ending_element = self.get_maybe_exist_named_child("ending")

        bartype = None
        if bartype_element:
            bartype = bartype_element.get_text()

        if repeat_element and hasattr(repeat_element, 'direction'):
            repeat = musicxml2ly_conversion.RepeatMarker()
            repeat.direction = {"forward": -1, "backward": 1}.get(
                repeat_element.direction, 0)

            if((repeat_element.direction == "forward" and bartype == "heavy-light") or
                    (repeat_element.direction == "backward" and bartype == "light-heavy")):
                bartype = None
            if hasattr(repeat_element, 'times'):
                try:
                    repeat.times = int(repeat_element.times)
                except ValueError:
                    repeat.times = 2
            repeat.event = self
            if repeat.direction == -1:
                retval[3] = repeat
            else:
                retval[1] = repeat

        if ending_element and hasattr(ending_element, 'type'):
            ending = musicxml2ly_conversion.EndingMarker()
            ending.direction = {"start": -1, "stop": 1, "discontinue": 1}.get(
                ending_element.type, 0)
            ending.event = self
            if ending.direction == -1:
                retval[4] = ending
            else:
                retval[0] = ending
            # TODO. ending number=""

        if bartype:
            b = musicexp.BarLine()
            b.type = bartype
            retval[2] = b

        return list(retval.values())


class Partial(Measure_element):
    def __init__(self, partial):
        Measure_element.__init__(self)
        self.partial = partial


class Stem(Music_xml_node):

    stem_value_dict = {
        'down': 'stemDown',
        'up': 'stemUp',
        'double': None,  # TODO: Implement
        'none': 'stemNeutral'
    }

    def to_stem_event(self):
        values = []
        value = self.stem_value_dict.get(self.get_text(), None)
        stem_value = musicexp.StemEvent()
        if value:
            stem_value.value = value
            values.append(stem_value)
        return values

    def to_stem_style_event(self):
        styles = []
        style_elm = musicexp.StemstyleEvent()
        if hasattr(self, 'color'):
            style_elm.color = utilities.hex_to_color(getattr(self, 'color'))
        if style_elm.color is not None:
            styles.append(style_elm)
        return styles


class Notehead(Music_xml_node):

    notehead_styles_dict = {
        'slash': '\'slash',
        'triangle': '\'triangle',
        'diamond': '\'diamond',
        'square': '\'la',  # TODO: Proper squared note head
        'cross': None,  # TODO: + shaped note head
        'x': '\'cross',
        'circle-x': '\'xcircle',
        'inverted triangle': None,  # TODO: Implement
        'arrow down': None,  # TODO: Implement
        'arrow up': None,  # TODO: Implement
        'slashed': None,  # TODO: Implement
        'back slashed': None,  # TODO: Implement
        'normal': None,
        'cluster': None,  # TODO: Implement
        'none': '#f',
        'do': '\'do',
        're': '\'re',
        'mi': '\'mi',
        'fa': '\'fa',
        'so': None,
        'la': '\'la',
        'ti': '\'ti',
    }

    def to_lily_object(self):  # function changed: additionally processcolor attribute
        styles = []

        # Notehead style
        key = self.get_text().strip()
        style = self.notehead_styles_dict.get(key, None)
        event = musicexp.NotestyleEvent()
        if style:
            event.style = style
        if hasattr(self, 'filled'):
            event.filled = (getattr(self, 'filled') == "yes")
        if hasattr(self, 'color'):
            event.color = utilities.hex_to_color(getattr(self, 'color'))
        if event.style or (event.filled is not None) or (event.color is not None):
            styles.append(event)
        # parentheses
        if hasattr(self, 'parentheses') and (self.parentheses == "yes"):
            styles.append(musicexp.ParenthesizeEvent())

        return styles


class Note(Measure_element):

    def __init__(self):
        Measure_element.__init__(self)
        self.instrument_name = ''
        self._after_grace = False
        self._duration = 1

    def is_grace(self):
        return self.get_maybe_exist_named_child('grace')

    def is_after_grace(self):
        if not self.is_grace():
            return False
        gr = self.get_maybe_exist_typed_child(Grace)
        return self._after_grace or hasattr(gr, 'steal-time-previous')

    def get_duration_log(self):
        ch = self.get_maybe_exist_named_child('type')

        if ch:
            log = ch.get_text().strip()
            return utilities.musicxml_duration_to_log(log)
        elif self.get_maybe_exist_named_child('grace'):
            # FIXME: is it ok to default to eight note for grace notes?
            return 3
        else:
            return None

    def get_duration_info(self):
        log = self.get_duration_log()
        if log is not None:
            dots = len(self.get_typed_children(Dot))
            return(log, dots)
        else:
            return None

    def get_factor(self):
        return 1

    def get_pitches(self):
        return self.get_typed_children(get_class('pitch'))

    def set_notehead_style(self, event):
        noteheads = self.get_named_children('notehead')
        for nh in noteheads:
            styles = nh.to_lily_object()
            for style in styles:
                event.add_associated_event(style)

    def set_stem_directions(self, event):
        stems = self.get_named_children('stem')
        for stem in stems:
            values = stem.to_stem_event()
            for v in values:
                event.add_associated_event(v)

    def set_stem_style(self, event):
        stems = self.get_named_children('stem')
        for stem in stems:
            styles = stem.to_stem_style_event()
            for style in styles:
                event.add_associated_event(style)

    def initialize_duration(self):
        from musicxml2ly_conversion import rational_to_lily_duration
        from musicexp import Duration
        # if the note has no Type child, then that method returns None. In that case,
        # use the <duration> tag instead. If that doesn't exist, either -> Error
        dur = self.get_duration_info()
        if dur:
            d = Duration()
            d.duration_log = dur[0]
            d.dots = dur[1]
            # Grace notes by specification have duration 0, so no time modification
            # factor is possible. It even messes up the output with *0/1
            if not self.get_maybe_exist_typed_child(Grace):
                d.factor = self._duration / d.get_length()
            return d
        else:
            if self._duration > 0:
                return rational_to_lily_duration(self._duration)
            else:
                self.message(
                    _("Encountered note at %s without type and duration(=%s)")
                    % (mxl_note.start, mxl_note._duration))
                return None

    def initialize_pitched_event(self):
        mxl_pitch = self.get_maybe_exist_typed_child(Pitch)
        pitch = mxl_pitch.to_lily_object()
        event = musicexp.NoteEvent()
        event.pitch = pitch
        acc = self.get_maybe_exist_named_child('accidental')
        if acc:
            # let's not force accs everywhere.
            event.cautionary = acc.cautionary
            # TODO: Handle editorial accidentals
            # TODO: Handle the level-display setting for displaying brackets/parentheses
        return event

    def initialize_unpitched_event(self):
        # Unpitched elements have display-step and can also have
        # display-octave.
        unpitched = self.get_maybe_exist_typed_child(Unpitched)
        event = musicexp.NoteEvent()
        event.pitch = unpitched.to_lily_object()
        return event

    def initialize_rest_event(self, convert_rest_positions=True):
        # rests can have display-octave and display-step, which are
        # treated like an ordinary note pitch
        rest = self.get_maybe_exist_typed_child(Rest)
        event = musicexp.RestEvent()
        if convert_rest_positions:
            pitch = rest.to_lily_object()
            event.pitch = pitch
        return event

    def to_lily_object(self,
                       convert_stem_directions=True,
                       convert_rest_positions=True):
        pitch = None
        duration = None
        event = None

        if self.get_maybe_exist_typed_child(Pitch):
            event = self.initialize_pitched_event()
        elif self.get_maybe_exist_typed_child(Unpitched):
            event = self.initialize_unpitched_event()
        elif self.get_maybe_exist_typed_child(Rest):
            event = self.initialize_rest_event(convert_rest_positions)
        else:
            self.message(_("cannot find suitable event"))

        if event:
            event.duration = self.initialize_duration()

        self.set_notehead_style(event)
        self.set_stem_style(event)
        if convert_stem_directions:
            self.set_stem_directions(event)

        return event


class Part_list(Music_xml_node):

    def __init__(self):
        Music_xml_node.__init__(self)
        self._id_instrument_name_dict = {}

    def generate_id_instrument_dict(self):

        # not empty to make sure this happens only once.
        mapping = {1: 1}
        for score_part in self.get_named_children('score-part'):
            for instr in score_part.get_named_children('score-instrument'):
                id = instr.id
                name = instr.get_named_child("instrument-name")
                mapping[id] = name.get_text()

        self._id_instrument_name_dict = mapping

    def get_instrument(self, id):
        if not self._id_instrument_name_dict:
            self.generate_id_instrument_dict()

        instrument_name = self._id_instrument_name_dict.get(id)
        if instrument_name:
            return instrument_name
        else:
            ly.warning(_("Unable to find instrument for ID=%s\n") % id)
            return "Grand Piano"


class Measure(Music_xml_node):

    def __init__(self):
        Music_xml_node.__init__(self)
        self.partial = 0

    def is_implicit(self):
        return hasattr(self, 'implicit') and self.implicit == 'yes'

    def get_notes(self):
        return self.get_typed_children(get_class('note'))


class Syllabic(Music_xml_node):

    def continued(self):
        text = self.get_text()
        return text == "begin" or text == "middle"

    def begin(self):
        return text == "begin"

    def middle(self):
        return text == "middle"

    def end(self):
        return text == "end"


class Lyric(Music_xml_node):

    def get_number(self):
        """
        Return the number attribute(if it exists) of the lyric element.

        @rtype: number
        @return: The value of the number attribute
        """
        if hasattr(self, 'number'):
            return int(self.number)
        else:
            return -1


class Sound(Music_xml_node):

    def get_tempo(self):
        """
        Return the tempo attribute(if it exists) of the sound element.
        This attribute can be used by musicxml2ly for the midi output(see L{musicexp.Score}).

        @rtype: string
        @return: The value of the tempo attribute
        """
        if hasattr(self, 'tempo'):
            return self.tempo
        else:
            return None


class Notations(Music_xml_node):

    def get_tie(self):
        ts = self.get_named_children('tied')
        starts = [t for t in ts if t.type == 'start']
        if starts:
            return starts[0]
        else:
            return None

    def get_tuplets(self):
        return self.get_typed_children(Tuplet)


class Time_modification(Music_xml_node):

    def get_fraction(self):
        b = self.get_maybe_exist_named_child('actual-notes')
        a = self.get_maybe_exist_named_child('normal-notes')
        return(int(a.get_text()), int(b.get_text()))

    def get_normal_type(self):
        tuplet_type = self.get_maybe_exist_named_child('normal-type')
        if tuplet_type:
            dots = self.get_named_children('normal-dot')
            log = utilities.musicxml_duration_to_log(
                tuplet_type.get_text().strip())
            return(log, len(dots))
        else:
            return None


class Accidental(Music_xml_node):

    def __init__(self):
        Music_xml_node.__init__(self)
        self.editorial = False
        self.cautionary = False


class Tuplet(Music_xml_spanner):

    def duration_info_from_tuplet_note(self, tuplet_note):
        tuplet_type = tuplet_note.get_maybe_exist_named_child('tuplet-type')
        if tuplet_type:
            dots = tuplet_note.get_named_children('tuplet-dot')
            log = utilities.musicxml_duration_to_log(
                tuplet_type.get_text().strip())
            return(log, len(dots))
        else:
            return None

    # Return tuplet note type as(log, dots)
    def get_normal_type(self):
        tuplet = self.get_maybe_exist_named_child('tuplet-normal')
        if tuplet:
            return self.duration_info_from_tuplet_note(tuplet)
        else:
            return None

    def get_actual_type(self):
        tuplet = self.get_maybe_exist_named_child('tuplet-actual')
        if tuplet:
            return self.duration_info_from_tuplet_note(tuplet)
        else:
            return None

    def get_tuplet_note_count(self, tuplet_note):
        if tuplet_note:
            tuplet_nr = tuplet_note.get_maybe_exist_named_child(
                'tuplet-number')
            if tuplet_nr:
                return int(tuplet_nr.get_text())
        return None

    def get_normal_nr(self):
        return self.get_tuplet_note_count(self.get_maybe_exist_named_child('tuplet-normal'))

    def get_actual_nr(self):
        return self.get_tuplet_note_count(self.get_maybe_exist_named_child('tuplet-actual'))


class Slur(Music_xml_spanner):

    def get_type(self):
        return self.type


class Tied(Music_xml_spanner):

    def get_type(self):
        return self.type


class Beam(Music_xml_spanner):
    def get_type(self):
        return self.get_text()

    def is_primary(self):
        if hasattr(self, 'number'):
            return self.number == "1"
        else:
            return True


class Octave_shift(Music_xml_spanner):
    # default is 8 for the octave-shift!
    def get_size(self):
        if hasattr(self, 'size'):
            return int(self.size)
        else:
            return 8


# Rests in MusicXML are <note> blocks with a <rest> inside. This class is only
# for the inner <rest> element, not the whole rest block.
class Rest(Music_xml_node):

    def __init__(self):
        Music_xml_node.__init__(self)
        self._is_whole_measure = False

    def is_whole_measure(self):
        return self._is_whole_measure

    def get_step(self):
        ch = self.get_maybe_exist_typed_child(get_class('display-step'))
        if ch:
            return ch.get_text().strip()
        else:
            return None

    def get_octave(self):
        ch = self.get_maybe_exist_typed_child(get_class('display-octave'))
        if ch:
            oct = ch.get_text().strip()
            return int(oct)
        else:
            return None

    def to_lily_object(self):
        p = None
        step = self.get_step()
        if step:
            p = musicexp.Pitch()
            p.step = musicxml2ly_conversion.musicxml_step_to_lily(step)
        octave = self.get_octave()
        if octave and p:
            p.octave = octave - 4
        return p


class Bend(Music_xml_node):

    def bend_alter(self):
        alter = self.get_maybe_exist_named_child('bend-alter')
        return utilities.interpret_alter_element(alter)


class ChordPitch(Music_xml_node):

    def step_class_name(self):
        return 'root-step'

    def alter_class_name(self):
        return 'root-alter'

    def get_step(self):
        ch = self.get_unique_typed_child(get_class(self.step_class_name()))
        return ch.get_text().strip()

    def get_alteration(self):
        ch = self.get_maybe_exist_typed_child(
            get_class(self.alter_class_name()))
        return utilities.interpret_alter_element(ch)


class Bass(ChordPitch):

    def step_class_name(self):
        return 'bass-step'

    def alter_class_name(self):
        return 'bass-alter'


class ChordModification(Music_xml_node):

    def get_type(self):
        ch = self.get_maybe_exist_typed_child(get_class('degree-type'))
        return {'add': 1, 'alter': 1, 'subtract': -1}.get(ch.get_text().strip(), 0)

    def get_value(self):
        ch = self.get_maybe_exist_typed_child(get_class('degree-value'))
        value = 0
        if ch:
            value = int(ch.get_text().strip())
        return value

    def get_alter(self):
        ch = self.get_maybe_exist_typed_child(get_class('degree-alter'))
        return utilities.interpret_alter_element(ch)


class Frame(Music_xml_node):

    def get_frets(self):
        return self.get_named_child_value_number('frame-frets', 4)

    def get_strings(self):
        return self.get_named_child_value_number('frame-strings', 6)

    def get_first_fret(self):
        return self.get_named_child_value_number('first-fret', 1)


class Frame_Note(Music_xml_node):

    def get_string(self):
        return self.get_named_child_value_number('string', 1)

    def get_fret(self):
        return self.get_named_child_value_number('fret', 0)

    def get_fingering(self):
        return self.get_named_child_value_number('fingering', -1)

    def get_barre(self):
        n = self.get_maybe_exist_named_child('barre')
        if n:
            return getattr(n, 'type', '')
        else:
            return ''


class Musicxml_voice:

    def __init__(self):
        self._elements = []
        self._staves = {}
        self._start_staff = None
        self._lyrics = []
        self._has_lyrics = False

    def add_element(self, e):
        self._elements.append(e)
        if(isinstance(e, Note)
                and e.get_maybe_exist_typed_child(Staff)):
            name = e.get_maybe_exist_typed_child(Staff).get_text()

            if not self._start_staff and not e.get_maybe_exist_typed_child(Grace):
                self._start_staff = name
            self._staves[name] = True

        lyrics = e.get_typed_children(Lyric)
        if not self._has_lyrics:
            self.has_lyrics = len(lyrics) > 0

        for l in lyrics:
            nr = l.get_number()
            if nr > 0 and nr not in self._lyrics:
                self._lyrics.append(nr)

    def insert(self, idx, e):
        self._elements.insert(idx, e)

    def get_lyrics_numbers(self):
        if(len(self._lyrics) == 0) and self._has_lyrics:
            # only happens if none of the <lyric> tags has a number attribute
            return [1]
        else:
            return self._lyrics


class Part(Music_xml_node):

    def __init__(self):
        Music_xml_node.__init__(self)
        self._voices = {}
        self._staff_attributes_dict = {}

    def get_part_list(self):
        n = self
        while n and n.get_name() != 'score-partwise':
            n = n._parent

        return n.get_named_child('part-list')

    def graces_to_aftergraces(self, pending_graces):
        for gr in pending_graces:
            gr._when = gr._prev_when
            gr._measure_position = gr._prev_measure_position
            gr._after_grace = True

    def interpret(self):
        """Set durations and starting points."""
        """The starting point of the very first note is 0!"""

        part_list = self.get_part_list()

        now = Fraction(0)
        factor = Fraction(1)
        attributes_dict = {}
        attributes_object = None
        measures = self.get_typed_children(Measure)
        last_moment = Fraction(-1)
        last_measure_position = Fraction(-1)
        measure_position = Fraction(0)
        measure_start_moment = now
        is_first_measure = True
        previous_measure = None
        # Graces at the end of a measure need to have their position set to the
        # previous number!
        pending_graces = []
        for m in measures:
            # implicit measures are used for artificial measures, e.g. when
            # a repeat bar line splits a bar into two halves. In this case,
            # don't reset the measure position to 0. They are also used for
            # upbeats(initial value of 0 fits these, too).
            # Also, don't reset the measure position at the end of the loop,
            # but rather when starting the next measure(since only then do we
            # know if the next measure is implicit and continues that measure)
            if not m.is_implicit():
                # Warn about possibly overfull measures and reset the position
                if attributes_object and previous_measure and previous_measure.partial == 0:
                    length = attributes_object.get_measure_length()
                    new_now = measure_start_moment + length
                    if now != new_now:
                        problem = 'incomplete'
                        if now > new_now:
                            problem = 'overfull'
                        # only for verbose operation.
                        if problem != 'incomplete' and previous_measure:
                            previous_measure.message(
                                '%s measure? Expected: %s, Difference: %s' % (problem, now, new_now - now))
                    now = new_now
                measure_start_moment = now
                measure_position = Fraction(0)

            voice_id = None
            assign_to_next_voice = []
            for n in m.get_all_children():
                # assign a voice to all measure elements
                if n.get_name() == 'backup':
                    voice_id = None

                if isinstance(n, Measure_element):
                    if n.get_voice_id():
                        voice_id = n.get_voice_id()
                        for i in assign_to_next_voice:
                            i.voice_id = voice_id
                        assign_to_next_voice = []
                    else:
                        if voice_id:
                            n.voice_id = voice_id
                        else:
                            assign_to_next_voice.append(n)

                # figured bass has a duration, but applies to the next note
                # and should not change the current measure position!
                if isinstance(n, FiguredBass):
                    n._divisions = factor.denominator
                    n._when = now
                    n._measure_position = measure_position
                    continue

                if isinstance(n, Hash_text):
                    continue
                dur = Fraction(0)

                if n.__class__ == Attributes:
                    n.set_attributes_from_previous(attributes_dict)
                    n.read_self()
                    attributes_dict = n._dict.copy()
                    attributes_object = n

                    # default to <divisions>1</divisions>
                    divisions = (int(attributes_dict['divisions'].get_text())
                        if 'divisions' in attributes_dict else 1)

                    factor = Fraction(1, divisions)


                if n.get_maybe_exist_typed_child(Duration):
                    mxl_dur = n.get_maybe_exist_typed_child(Duration)
                    dur = mxl_dur.get_length() * factor

                    if n.get_name() == 'backup':
                        dur = -dur
                        # reset all graces before the backup to after-graces:
                        self.graces_to_aftergraces(pending_graces)
                        pending_graces = []
                    if n.get_maybe_exist_typed_child(Grace):
                        dur = Fraction(0)

                    rest = n.get_maybe_exist_typed_child(Rest)
                    if(rest
                            and attributes_object
                            and attributes_object.get_measure_length() == dur):

                        rest._is_whole_measure = True

                if(dur > Fraction(0)
                        and n.get_maybe_exist_typed_child(Chord)):
                    now = last_moment
                    measure_position = last_measure_position

                n._when = now
                n._measure_position = measure_position

                # For all grace notes, store the previous note,  in case need
                # to turn the grace note into an after-grace later on!
                if isinstance(n, Note) and n.is_grace():
                    n._prev_when = last_moment
                    n._prev_measure_position = last_measure_position
                # After-graces are placed at the same position as the previous note
                if isinstance(n, Note) and n.is_after_grace():
                    # TODO: We should do the same for grace notes at the end of
                    # a measure with no following note!!!
                    n._when = last_moment
                    n._measure_position = last_measure_position
                elif isinstance(n, Note) and n.is_grace():
                    pending_graces.append(n)
                elif dur > Fraction(0):
                    pending_graces = []

                n._duration = dur
                if dur > Fraction(0):
                    last_moment = now
                    last_measure_position = measure_position
                    now += dur
                    measure_position += dur
                elif dur < Fraction(0):
                    # backup element, reset measure position
                    now += dur
                    measure_position += dur
                    if measure_position < 0:
                        # backup went beyond the measure start => reset to 0
                        now -= measure_position
                        measure_position = 0
                    last_moment = now
                    last_measure_position = measure_position
                if n._name == 'note':
                    instrument = n.get_maybe_exist_named_child('instrument')
                    if instrument:
                        n.instrument_name = part_list.get_instrument(
                            instrument.id)

            # reset all graces at the end of the measure to after-graces:
            self.graces_to_aftergraces(pending_graces)
            pending_graces = []
            # Incomplete first measures are not padded, but registered as partial
            if is_first_measure:
                is_first_measure = False
                # upbeats are marked as implicit measures
                if attributes_object and m.is_implicit():
                    length = attributes_object.get_measure_length()
                    measure_end = measure_start_moment + length
                    if measure_end != now:
                        m.partial = now
            previous_measure = m

    # modify attributes so that only those applying to the given staff remain
    def extract_attributes_for_staff(part, attr, staff):
        attributes = copy.copy(attr)
        attributes._children = []
        attributes._dict = attr._dict.copy()
        attributes._original_tag = attr
        # copy only the relevant children over for the given staff
        if staff == "None":
            staff = "1"
        for c in attr._children:
            if ((not hasattr(c, 'number') or c.number == staff) and
                  not isinstance(c, Hash_text)):
                attributes._children.append(c)
        if not attributes._children:
            return None
        else:
            return attributes

    def extract_voices(part):
        # The last indentified voice
        last_voice = None

        voices = OrderedDict()
        measures = part.get_typed_children(Measure)
        elements = []
        for m in measures:
            if m.partial > 0:
                elements.append(Partial(m.partial))
            elements.extend(m.get_all_children())
        # make sure we know all voices already so that dynamics, clefs, etc.
        # can be assigned to the correct voices
        voice_to_staff_dict = {}
        for n in elements:
            voice_id = n.get_maybe_exist_named_child('voice')
            vid = None
            if voice_id:
                vid = voice_id.get_text()
            elif isinstance(n, Note):
                # TODO: Check whether we shall really use "None" here, or
                #       rather use "1" as the default?
                if n.get_maybe_exist_named_child('chord'):
                    vid = last_voice
                else:
                    vid = "1"

            if vid is not None:
                last_voice = vid

            staff_id = n.get_maybe_exist_named_child('staff')
            sid = None
            if staff_id:
                sid = staff_id.get_text()
            else:
                # TODO: Check whether we shall really use "None" here, or
                #       rather use "1" as the default?
                #       If this is changed, need to change the corresponding
                #       check in extract_attributes_for_staff, too.
                sid = "None"
            if vid and vid not in voices:
                voices[vid] = Musicxml_voice()
            if vid and sid and not n.get_maybe_exist_typed_child(Grace):
                if vid not in voice_to_staff_dict:
                    voice_to_staff_dict[vid] = sid

        # invert the voice_to_staff_dict into a staff_to_voice_dict(since we
        # need to assign staff-assigned objects like clefs, times, etc. to
        # all the correct voices. This will never work entirely correct due
        # to staff-switches, but that's the best we can do!
        staff_to_voice_dict = {}
        for(v, s) in list(voice_to_staff_dict.items()):
            if s not in staff_to_voice_dict:
                staff_to_voice_dict[s] = [v]
            else:
                staff_to_voice_dict[s].append(v)

        start_attr = None
        assign_to_next_note = []
        id = None
        for n in elements:
            voice_id = n.get_maybe_exist_typed_child(get_class('voice'))
            if voice_id:
                id = voice_id.get_text()
            else:
                if n.get_maybe_exist_typed_child(get_class('chord')):
                    id = last_voice
                else:
                    id = "1"

            if id != "None":
                last_voice = id

            # We don't need backup/forward any more, since we have already
            # assigned the correct onset times.
            # TODO: Let Grouping through. Also: link, print, bokmark sound
            if not(isinstance(n, Note) or isinstance(n, Attributes) or
                    isinstance(n, Direction) or isinstance(n, Partial) or
                    isinstance(n, Barline) or isinstance(n, Harmony) or
                    isinstance(n, FiguredBass) or isinstance(n, Print)):
                continue

            if isinstance(n, Attributes) and not start_attr:
                start_attr = n
                continue

            if isinstance(n, Attributes):
                # assign these only to the voices they really belong to!
                for(s, vids) in list(staff_to_voice_dict.items()):
                    staff_attributes = part.extract_attributes_for_staff(n, s)
                    if staff_attributes:
                        for v in vids:
                            voices[v].add_element(staff_attributes)
                continue

            if isinstance(n, Partial) or isinstance(n, Barline) or isinstance(n, Print):
                for v in list(voices.keys()):
                    voices[v].add_element(n)
                continue

            if isinstance(n, Direction):
                if n.voice_id:
                    voices[n.voice_id].add_element(n)
                else:
                    assign_to_next_note.append(n)
                continue

            if isinstance(n, Harmony) or isinstance(n, FiguredBass):
                # store the harmony or figured bass element until we encounter
                # the next note and assign it only to that one voice.
                assign_to_next_note.append(n)
                continue

            if hasattr(n, 'print-object') and getattr(n, 'print-object') == "no":
                # Skip this note.
                pass
            else:
                for i in assign_to_next_note:
                    voices[id].add_element(i)
                assign_to_next_note = []
                voices[id].add_element(n)

        # Assign all remaining elements from assign_to_next_note to the voice
        # of the previous note:
        for i in assign_to_next_note:
            voices[id].add_element(i)
        assign_to_next_note = []

        if start_attr:
            for(s, vids) in list(staff_to_voice_dict.items()):
                staff_attributes = part.extract_attributes_for_staff(
                    start_attr, s)
                staff_attributes.read_self()
                part._staff_attributes_dict[s] = staff_attributes
                for v in vids:
                    voices[v].insert(0, staff_attributes)
                    voices[v]._elements[0].read_self()

        part._voices = voices

    def get_voices(self):
        return self._voices

    def get_staff_attributes(self):
        return self._staff_attributes_dict


class BarStyle(Music_xml_node):
    pass


class BeatType(Music_xml_node):
    pass


class BeatUnit(Music_xml_node):
    pass


class BeatUnitDot(Music_xml_node):
    pass


class Beats(Music_xml_node):
    pass


class Bracket(Music_xml_spanner):
    pass


class Chord(Music_xml_node):
    pass


class Dashes(Music_xml_spanner):
    pass


class DirType(Music_xml_node):
    pass


class Direction(Measure_element):
    pass


class Dot(Music_xml_node):
    pass


class Elision(Music_xml_node):
    pass


class Extend(Music_xml_node):
    pass


class FiguredBass(Music_xml_node):
    pass


class Glissando(Music_xml_spanner):
    pass


class Grace(Music_xml_node):
    pass


class Harmony(Music_xml_node):
    pass


class Hash_comment(Music_xml_node):
    pass


class KeyAlter(Music_xml_node):
    pass


class Direction (Measure_element):
    pass


class KeyOctave(Music_xml_node):
    pass


class KeyStep(Music_xml_node):
    pass


class Part_group(Music_xml_node):
    pass


class Pedal(Music_xml_spanner):
    pass


class PerMinute(Music_xml_node):
    pass


class Print(Music_xml_node):
    pass


class Root(ChordPitch):
    pass


class Score_part(Music_xml_node):
    pass


class Slide(Music_xml_spanner):
    pass


class Staff(Music_xml_node):
    pass


class Text(Music_xml_node):
    pass


class Type(Music_xml_node):
    pass


class Wavy_line(Music_xml_spanner):
    pass


class Wedge(Music_xml_spanner):
    pass


class Words(Music_xml_node):
    pass


# need this, not all classes are instantiated
# for every input file. Only add those classes, that are either directly
# used by class name or extend Music_xml_node in some way!
class_dict = {
    '#comment': Hash_comment,
    '#text': Hash_text,
    'accidental': Accidental,
    'attributes': Attributes,
    'barline': Barline,
    'bar-style': BarStyle,
    'bass': Bass,
    'beam': Beam,
    'beats': Beats,
    'beat-type': BeatType,
    'beat-unit': BeatUnit,
    'beat-unit-dot': BeatUnitDot,
    'bend': Bend,
    'bracket': Bracket,
    'chord': Chord,
    'credit': Credit,
    'dashes': Dashes,
    'degree': ChordModification,
    'dot': Dot,
    'direction': Direction,
    'direction-type': DirType,
    'duration': Duration,
    'elision': Elision,
    'extend': Extend,
    'frame': Frame,
    'frame-note': Frame_Note,
    'figured-bass': FiguredBass,
    'glissando': Glissando,
    'grace': Grace,
    'harmony': Harmony,
    'identification': Identification,
    'key-alter': KeyAlter,
    'key-octave': KeyOctave,
    'key-step': KeyStep,
    'lyric': Lyric,
    'measure': Measure,
    'notations': Notations,
    'note': Note,
    'notehead': Notehead,
    'octave-shift': Octave_shift,
    'part': Part,
    'part-group': Part_group,
    'part-list': Part_list,
    'pedal': Pedal,
    'per-minute': PerMinute,
    'pitch': Pitch,
    'print': Print,
    'rest': Rest,
    'root': Root,
    'score-part': Score_part,
    'slide': Slide,
    'slur': Slur,
    'sound': Sound,
    'staff': Staff,
    'stem': Stem,
    'syllabic': Syllabic,
    'text': Text,
    'time-modification': Time_modification,
    'tied': Tied,
    'tuplet': Tuplet,
    'type': Type,
    'unpitched': Unpitched,
    'wavy-line': Wavy_line,
    'wedge': Wedge,
    'words': Words,
    'work': Work,
}


def name2class_name(name):
    name = name.replace('-', '_')
    name = name.replace('#', 'hash_')
    name = name[0].upper() + name[1:].lower()

    return str(name)


def get_class(name):
    classname = class_dict.get(name)
    if classname:
        return classname
    else:
        class_name = name2class_name(name)
        klass = type(class_name, (Music_xml_node,), {})
        class_dict[name] = klass
        return klass


def lxml_demarshal_node(node):
    name = node.tag

    # Ignore comment nodes, which are also returned by the etree parser!
    if name is None or node.__class__.__name__ == "_Comment":
        return None
    klass = get_class(name)
    py_node = klass()

    py_node._original = node
    py_node._name = name
    py_node._data = node.text
    py_node._children = [lxml_demarshal_node(cn) for cn in node.getchildren()]
    py_node._children = [x for x in py_node._children if x]

    for c in py_node._children:
        c._parent = py_node

    for(k, v) in list(node.items()):
        py_node.__dict__[k] = v
        py_node._attribute_dict[k] = v

    return py_node


def minidom_demarshal_node(node):
    name = node.nodeName

    klass = get_class(name)
    py_node = klass()
    py_node._name = name
    py_node._children = [minidom_demarshal_node(cn) for cn in node.childNodes]
    for c in py_node._children:
        c._parent = py_node

    if node.attributes:
        for(nm, value) in list(node.attributes.items()):
            py_node.__dict__[nm] = value
            py_node._attribute_dict[nm] = value

    py_node._data = None
    if node.nodeType == node.TEXT_NODE and node.data:
        py_node._data = node.data

    py_node._original = node
    return py_node


if __name__ == '__main__':
    import lxml.etree

    tree = lxml.etree.parse('beethoven.xml')
    mxl_tree = lxml_demarshal_node(tree.getroot())
    ks = sorted(class_dict.keys())
    print('\n'.join(ks))
