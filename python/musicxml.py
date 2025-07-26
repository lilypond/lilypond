# musicxml.py
# -*- coding: utf-8 -*-
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2005--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>,
# Copyright (C) 2007--2023 Reinhold Kainhofer <reinhold@kainhofer.com>
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
import musicxml2ly_conversion as conversion
import utilities

from dllist import dllist


# To work around LilyPond's infamous issue #34, store the (musical) lengths
# of grace note sequences at the beginning of a voice (i.e., at moment zero)
# in a global dictionary.
starting_grace_lengths = {}
max_starting_grace_length = 0


def minidom_demarshal_text_to_int(node):
    text = ''.join([n.data for n in node.childNodes
                    if n.nodeType == node.TEXT_NODE])
    return int(text)


def minidom_demarshal_text_to_int_or_float(node):
    text = ''.join([n.data for n in node.childNodes
                    if n.nodeType == node.TEXT_NODE])
    try:
        return int(text)
    except ValueError:
        pass
    return float(text)


def minidom_demarshal_text_to_str(node):
    text = ''.join([n.data for n in node.childNodes
                    if n.nodeType == node.TEXT_NODE])
    return text.strip()


def minidom_demarshal_true(node):
    return True


class Xml_node(object):
    _name = 'xml_node'

    def __init__(self):
        self._children = []
        self._content = {}  # certain children are here instead of _children
        self._data = None
        self._attribute_dict = {}

    def __contains__(self, key):
        return key in self._content

    # Xml_node presents itself as a dictionary of children keyed by MusicXML
    # element name.  The class variable `max_occurs_by_child` specifies
    # which elements to track with this dictionary.
    #
    # When `max_occurs_by_child` is 1, a successful lookup accesses the
    # child node.  For optional elements, the lookup may fail.
    #
    # When `max_occurs_by_child` is 2 (meaning not limited) the lookup
    # accesses a list of child nodes, which may be empty.  The class
    # specifying `max_occurs_by_child 2` is responsible for initializing an
    # empty list in its instances so that the lookup cannot fail.
    def __getitem__(self, key):
        return self._content[key]

    def get(self, key, default_value=None):
        return self._content.get(key, default_value)

    def get_parent(self):
        return self._parent

    @classmethod
    def get_name(cls):
        return cls._name

    def get_text(self):
        if self._data:
            return self._data

        if not self._children:
            return ''

        return ''.join([c.get_text() for c in self._children])

    def message(self, msg):
        ly.warning(msg + ':')

        p = self
        while p:
            ly.progress('  In: <%s %s>' % (p._name, ' '.join(
                ['%s=%s' % item for item in p._attribute_dict.items()])))
            p = p.get_parent()

    def dump(self, indent=''):
        ly.debug_output('%s<%s%s>' % (indent, self._name, ''.join(
            [' %s=%s' % item for item in self._attribute_dict.items()])))
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


# This class gets injected by `musicxml2ly` to handle chains of
# `<direction-type>` children.
class LilyPond_markup(Xml_node):
    _name = 'lilypond-markup'

    def __init__(self):
        Xml_node.__init__(self)


class Music_xml_node(Xml_node):
    def __init__(self):
        Xml_node.__init__(self)
        self._when = None
        self._duration = None
        self.converted = False
        self.voice_id = None


class Music_xml_spanner(Music_xml_node):
    def __init__(self):
        Music_xml_node.__init__(self)
        self.paired_with = None
        self.spanner_event = None  # For linking with `musicexp` nodes.

    def get_type(self):
        # Most subclasses represent elements with a required 'type' attribute.
        return self.type


class Measure_element(Music_xml_node):
    def get_voice_id(self):
        return self.get('voice', self.voice_id)


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
            # If this node has a 'type' attribute such as `type="words"`,
            # include it in the return value.  Otherwise it is assumed that
            # the text contents of this node looks something like this:
            # 'Copyright: X.Y.' and thus already contains the relevant
            # information.
            rights_type = getattr(r, 'type', None)
            if rights_type is not None:
                rights_type = rights_type.title()  # capitalize first letter
                result = rights_type + ': ' + text
                ret.append(result)
            else:
                ret.append(text)
        return "\n".join(ret)

    def get_source(self):
        source = self.get_maybe_exist_named_child('source')
        if source:
            return source.get_text()
        return ''

    def get_creator(self, type):
        creators = self.get_named_children('creator')
        ret = []
        for i in creators:
            if getattr(i, 'type', None) == type:
                text = i.get_text()
                if text:
                    ret.append(text)
        return '\n'.join(ret)

    def get_composer(self):
        c = self.get_creator('composer')
        if c:
            return c

        # XXX Why a heuristic second try?
        creators = self.get_named_children('creator')
        # Return the first `<creator>` element that has no type.
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
        encoding = self.get_maybe_exist_named_child('encoding')
        if encoding:
            children = encoding.get_named_children(type)
            ret = []
            for child in children:
                text = child.get_text()
                if text:
                    ret.append(text)
            return '\n'.join(ret)
        return ''

    def get_encoding_software(self):
        return self.get_encoding_information('software')

    def get_encoding_date(self):
        return self.get_encoding_information('encoding-date')

    def get_encoding_person(self):
        return self.get_encoding_information('encoder')

    def get_encoding_description(self):
        return self.get_encoding_information('encoding-description')

    def get_file_description(self):
        misc = self.get_maybe_exist_named_child('miscellaneous')
        if misc:
            misc_fields = misc.get_named_children('miscellaneous-field')
            for mf in misc_fields:
                if getattr(mf, 'name', None) == 'description':
                    return mf.get_text()
        return ''


class Credit_group:
    def __init__(self, credits):
        self.words_font_sizes = []
        self.words_default_xs = []
        self.words_default_ys = []

        # Collect 'font-size', 'default-x', and 'default-y' attribute values
        # of the first `<credit-words>` child of all `<credit>` elements.
        for cred in credits:
            words = cred.get_first_credit_words()

            text = getattr(words, 'font-size', None)
            if text is not None:
                self.words_font_sizes.append(int(float(text)))

            text = getattr(words, 'default-x', None)
            if text is not None:
                self.words_default_xs.append(round(float(text)))

            text = getattr(words, 'default-y', None)
            if text is not None:
                self.words_default_ys.append(round(float(text)))

        self.words_font_sizes.sort(reverse=True)
        # Coordinates are relative to the bottom-left corner of a page.
        self.words_default_xs.sort(reverse=True)
        self.words_default_ys.sort(reverse=True)


class Credit(Xml_node):
    max_occurs_by_child = {
        'credit-words': 2,
    }

    def __init__(self):
        Xml_node.__init__(self)
        self._content['credit-words'] = []

    def get_type(self):
        types = self.get_named_children('credit-type')
        val = []
        for t in types:
            val.append(t.get_text())
        # The choice of using ', ' as a separator between multiple credit
        # types in the return value is arbitrary.
        return ', '.join(val)

    def get_first_credit_words(self):
        try:
            return self['credit-words'][0]
        except IndexError:
            return None

    # Apply heuristics to find out where a `<credit>` element is positioned
    # on a page and what it does, then try to derive a proper type for it.
    def find_type(self, credit_group):
        # Collect various' attribute values of the first `<credit-words>`
        # child of the current `<credit>` element.
        words = self.get_first_credit_words()

        size = getattr(words, 'font-size', None)
        if size is not None:
            size = int(float(size))

        x = getattr(words, 'default-x', None)
        if x is not None:
            x = round(float(x))

        y = getattr(words, 'default-y', None)
        if y is not None:
            y = round(float(y))

        justify = getattr(words, 'justify', 'left')
        # The standard says that if the 'halign' attribute is not present,
        # it takes its value from the 'justify' attribute.
        halign = getattr(words, 'halign', justify)
        valign = getattr(words, 'valign', None)

        # The arrays in `credit_group` are sorted in reverse order.
        if (size and size == credit_group.words_font_sizes[0]
                and y and y == credit_group.words_default_ys[0]
                and halign == 'center'):
            return 'title'
        elif (y and y > credit_group.words_default_ys[-1]
              and y < credit_group.words_default_ys[0]
              and halign == 'center'):
            return 'subtitle'
        elif (halign == 'left'
              and (not x or x == credit_group.words_default_xs[-1])):
            return 'lyricist'
        elif (halign == 'right'
              and (not x or x == credit_group.words_default_xs[0])):
            return 'composer'
        elif (size and size == credit_group.words_font_sizes[-1]
              and y == credit_group.words_default_ys[-1]):
            return 'rights'

        # Special cases for Finale NotePad.
        elif (valign and valign == 'top'
              and y and y == credit_group.words_default_ys[1]):
            return 'subtitle'
        elif (valign and valign == 'top'
              and x and x == credit_group.words_default_xs[-1]):
            return 'lyricist'
        elif (valign and valign == 'top'
              and y and y == credit_group.words_default_ys[-1]):
            return 'rights'

        # Other special cases.
        elif valign and valign == 'bottom':
            return 'rights'
        elif len([i for i, item in enumerate(credit_group.words_default_ys)
                 if item == y]) == 2:
            # The first one is the composer, the second one is the lyricist.
            return 'composer'

        return ''  # No type recognized.


class Duration(Music_xml_node):
    minidom_demarshal_to_value = minidom_demarshal_text_to_int


class Hash_text(Music_xml_node):
    def dump(self, indent=''):
        ly.debug_output(self._data.strip())


class Pitch(Music_xml_node):
    max_occurs_by_child = {
        'alter': 1,
        'octave': 1,
        'step': 1,
    }

    def to_lily_object(self):
        p = musicexp.Pitch()
        p.alteration = self.get('alter', 0)
        p.step = conversion.musicxml_step_to_lily(self['step'])
        p.octave = self['octave'] - 4
        return p


class Unpitched(Music_xml_node):
    max_occurs_by_child = {
        'display-octave': 1,
        'display-step': 1,
    }

    def to_lily_object(self, clef):
        p = musicexp.Pitch()
        step = self.get('display-step')
        if step:
            p.step = conversion.musicxml_step_to_lily(step)
            # If `<display-step>` is present, `<display-octave>` must be
            # present, too.
            p.octave = self['display-octave'] - 4
        else:
            # We have to position the note on the middle line (or gap) of
            # the staff.
            p = clef.pitch
        return p


class Alter(Music_xml_node):
    minidom_demarshal_to_value = minidom_demarshal_text_to_int_or_float


class Arpeggiate(Music_xml_node):
    pass


class Attributes(Measure_element):
    def __init__(self):
        Measure_element.__init__(self)
        self._dict = {}
        self._original_tag = None
        self._time_signature_cache = None

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
        if not isinstance(sig[0], list) and sig[0] < 0:
            return -1

        if isinstance(sig[0], list):
            # Complex compound time signature.
            l = 0
            for i in sig:
                l += self.single_time_sig_to_fraction(i)
            return l
        else:
            # Simple (maybe compound) time signature of the form
            # `[beat, ..., type]`.
            return self.single_time_sig_to_fraction(sig)
        return 0

    # Return time signature as a
    #
    #   [beat, beat-type]
    #
    # list (a negative value for `beat` indicates a 'senza misura' time
    # signature).  For compound signatures, return either
    #
    #   [beat, beat, ..., beat-type]
    #
    # or
    #
    #   [[beat, ..., beat-type], [beat, ..., beat-type], ...]
    def get_time_signature(self):
        if self._time_signature_cache:
            return self._time_signature_cache

        try:
            mxl = self.get_named_attribute('time')
            if not mxl:
                return None

            # `senza_misura_length` is set by `Part.interpret()`; if not set
            # yet we use a dummy value.
            sml = [-mxl.senza_misura_length.numerator,
                   mxl.senza_misura_length.denominator]
            if sml[0]:
                self._time_signature_cache = sml
                return sml
            elif mxl.get_maybe_exist_named_child('senza-misura'):
                return [-4, 4]
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
        except (KeyError, ValueError):
            self.message(
                _("Unable to interpret time signature! Falling back to 4/4."))
            return [4, 4]

    # Return clef information in the form
    #
    #   ["cleftype", position, octave-shift, color, font-size, print-object]
    #
    # or `None` if there is no `<clef>` element.
    def get_clef_information(self):
        mxl = self.get_maybe_exist_named_child('clef')
        if not mxl:
            return None

        clefinfo = [None, None, None, None, None, True]
        sign = mxl.get_maybe_exist_named_child('sign')
        if sign:
            clefinfo[0] = sign.get_text()
        line = mxl.get_maybe_exist_named_child('line')
        if line:
            clefinfo[1] = int(line.get_text())
        octave = mxl.get_maybe_exist_named_child('clef-octave-change')
        if octave:
            clefinfo[2] = int(octave.get_text())
        else:
            clefinfo[2] = 0
        clefinfo[3] = getattr(mxl, 'color', None)
        clefinfo[4] = getattr(mxl, 'font-size', None)
        clefinfo[5] = (getattr(mxl, 'print-object', 'yes') == 'yes')
        return clefinfo

    def get_key_signature(self):
        """
        Return `(key_sig, color, font_size, visible)`.  Value `key_sig` is a
        tuple `(fifths, mode)` if the key signature is given as major/minor
        in the Circle of Fifths.  Otherwise it is an alterations list of the
        form `[[step, alter, octave], [step, alter, octave], ...]` where the
        `octave` values are optional.
        """
        key = self.get_named_attribute('key')
        if not key:
            return None

        color = getattr(key, 'color', None)
        font_size = getattr(key, 'font-size', None)
        visible = (getattr(key, 'print-object', 'yes') == 'yes')

        fifths_elm = key.get_maybe_exist_named_child('fifths')
        if fifths_elm:
            mode_node = key.get_maybe_exist_named_child('mode')
            mode = None
            if mode_node:
                mode = mode_node.get_text()
            if not mode or mode == '' or mode == 'none':
                mode = 'major'
            fifths = int(fifths_elm.get_text())
            # TODO: Shall we try to convert the key-octave, too?
            key_sig = (fifths, mode)
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
                    nr = int(getattr(i, 'number', -1))
                    if nr > 0 and nr <= len(alterations):
                        # MusicXML Octave 4 is middle C -> shift to 0
                        alterations[nr - 1].append(int(i.get_text()) - 4)
                    else:
                        i.message(_("Key alteration octave given for a "
                                    "non-existing alteration nr. %s, "
                                    "available numbers: %s!") %
                                  (nr, len(alterations)))
            key_sig = alterations

        return (key_sig, color, font_size, visible)

    def get_cancellation(self):
        key = self.get_named_attribute('key')
        if not key:
            return None
        cancel_elm = key.get_maybe_exist_named_child('cancel')
        if cancel_elm:
            cancel = int(cancel_elm.get_text())
            location = getattr(cancel_elm, 'location', 'left')
            return (cancel, location)
        else:
            return None

    def get_transposition(self):
        return self.get_named_attribute('transpose')


class Backup(Measure_element):
    max_occurs_by_child = {
        'duration': 1,
    }


class Barline(Measure_element):
    def to_lily_object(self):
        # Return a list of two lists.
        #
        # * The first one contains at most three elements, in the following
        #   order.
        #
        #     bar-line
        #     backward-marker
        #     forward-marker
        #
        #   Both `backward-marker` and `forward-marker` elements are either
        #   of type `RepeatMarker`, `EndingMarker`, or `RepeatEndingMarker`.
        #
        #   The `bar-line` element is of type `BarLine`; by putting it
        #   before `backward-marker` it gets included into the repeat group
        #   (in function `musicxml2ly.group_repeats`) so that it can control
        #   a repeat bar line's attributes.
        #
        #   Missing elements are omitted.
        #
        # * The second list has at most two elements, holding fermata nodes
        #   attached to the bar line.
        #
        retval = [None, None, None, None, None]
        bartype_element = self.get_maybe_exist_named_child("bar-style")
        repeat_element = self.get_maybe_exist_named_child("repeat")
        ending_element = self.get_maybe_exist_named_child("ending")
        fermata_elements = self.get_named_children("fermata")

        bartype = None
        barline_color = None
        if bartype_element:
            bartype = bartype_element.get_text()
            barline_color = getattr(bartype_element, 'color', None)

        direction = getattr(repeat_element, 'direction', None)
        if direction is not None:
            repeat = conversion.RepeatMarker()
            repeat.direction = {'forward': -1, 'backward': 1}.get(direction, 0)
            repeat.at_start = (repeat.direction == -1 and self._when == 0)

            # The MusicXML standard has issues with specifying the type for
            # repeats, especially for back-to-back repeats; see
            # https://github.com/w3c/musicxml/issues/104 – we do what Finale
            # does, namely to ignore the bar type for start and end repeats,
            # setting them to 'thick-thin' and 'thin-thick', respectively.
            # Since this is LilyPond's default, we don't have to do
            # anything.
            #
            # Similarly, the default back-to-back repeat type for Finale is
            # 'thin-thick-thin', and we set up LilyPond to use this as the
            # default, too, which means that we don't have to do anything
            # for this either.  The only case we actually handle is
            # 'heavy-heavy', assuming that it specifies a 'heavy-heavy' type
            # for a back-to-back repeat bar line.
            if bartype == 'heavy-heavy':
                bartype = 'dots-heavy-heavy-dots'
            else:
                bartype = None
            times = getattr(repeat_element, 'times', None)
            if times is not None:
                try:
                    repeat.times = int(times)
                except ValueError:
                    pass
            if repeat.direction == -1:
                retval[3] = repeat
            else:
                retval[2] = repeat

        ending_type = getattr(ending_element, 'type', None)
        if ending_type is not None:
            ending = conversion.EndingMarker()
            ending.direction = {"start": -1, "stop": 1, "discontinue": 1}.get(
                ending_type, 0)
            ending.mxl_event = ending_element

            ending_number = getattr(ending_element, 'number', None)
            if ending_number is not None:
                ending.volte = \
                    conversion.musicxml_numbers_to_volte(ending_number)

            if ending.direction == -1:
                retval[4] = ending
            else:
                retval[1] = ending

        if bartype is not None or barline_color is not None:
            b = musicexp.BarLine()
            b.type = bartype
            b.color = barline_color
            retval[0] = b

        # Synthesize `RepeatEndingMarker` objects if possible.
        if retval[2] is not None and retval[1] is not None:
            retval[2] = conversion.RepeatEndingMarker(retval[2], retval[1])
            retval[1] = None
        if retval[3] is not None and retval[4] is not None:
            retval[3] = conversion.RepeatEndingMarker(retval[3], retval[4])
            retval[4] = None

        return [[r for r in retval if r is not None], fermata_elements[:2]]


class Partial(Measure_element):
    def __init__(self, partial):
        Measure_element.__init__(self)
        self.partial = partial


class Stem(Music_xml_node):
    def to_stem_event(self, note_color=None, is_rest=False,
                      convert_stem_directions=True):
        event = musicexp.StemEvent()
        # MusicXML 4.0 doesn't provide a means to control the color of the
        # flag separately.  In LilyPond, `Stem.color` by default controls
        # the color of the flag, too.
        event.color = getattr(self, 'color', note_color)

        event.is_stemlet = is_rest

        value = self.get_text().strip()
        # Only catch 'up' and 'down' with the command-line option.
        if convert_stem_directions or value == 'none':
            event.value = value

        if (event.value is not None
                or event.color is not None
                or event.is_stemlet):
            return event

        return None


class NonArpeggiate(Music_xml_node):
    pass


class Notehead(Music_xml_node):
    def to_lily_object(self, note_color=None, note_font_size=None):
        styles = []

        # Note head style.
        event = musicexp.NotestyleEvent()
        event.style = self.get_text().strip()
        event.color = getattr(self, 'color', note_color)
        event.font_size = getattr(self, 'font-size', note_font_size)
        event.duration = self._duration
        event.filled = getattr(self, 'filled', None)

        if (event.style
                or event.filled is not None
                or event.color is not None
                or event.font_size is not None):
            styles.append(event)

        # Parentheses.
        if getattr(self, 'parentheses', None) == 'yes':
            styles.append(musicexp.ParenthesizeEvent())

        return styles


class Note(Measure_element):
    max_occurs_by_child = {
        'accidental': 1,
        'beam': 2,
        'chord': 1,
        'dot': 2,
        'duration': 1,
        'grace': 1,
        'instrument': 2,
        'lyric': 2,
        'notations': 2,
        'notehead': 1,
        'pitch': 1,
        'rest': 1,
        'staff': 1,
        'stem': 1,
        'type': 1,
        'unpitched': 1,
        'voice': 1,
    }

    def __init__(self):
        Measure_element.__init__(self)
        self.instrument_name = ''
        self.single_voice = None  # Not set for invisible and/or chord notes.
        self._after_grace = False
        self._duration = 1
        self._content['beam'] = []
        self._content['dot'] = []
        self._content['instrument'] = []
        self._content['lyric'] = []
        self._content['notations'] = []

    def is_after_grace(self):
        grace = self.get('grace', False)
        return grace and (self._after_grace
                          or hasattr(grace, 'steal-time-previous'))

    def get_duration_log(self):
        # <type> is optional, but profiling showed that is slightly better to
        # treat it as expected.
        try:
            ch = self['type']
        except KeyError:
            # FIXME: is it ok to default to eight note for grace notes?
            return 3 if 'grace' in self else None
        log = ch.get_text().strip()
        return utilities.musicxml_duration_to_log(log)

    def get_duration_info(self):
        log = self.get_duration_log()
        return (log, len(self['dot'])) if log is not None else None

    def get_factor(self):
        return 1

    def initialize_duration(self):
        from musicexp import Duration
        # If the note has no Type child return None.  In that case, use the
        # <duration> tag instead.  If that doesn't exist either, report an
        # error.
        dur = self.get_duration_info()
        if dur:
            d = Duration()
            d.duration_log = dur[0]
            d.dots = dur[1]
            # Grace notes by specification have duration 0, so no time
            # modification factor is possible.  It even messes up the output
            # with *0/1.
            if 'grace' not in self:
                nominal_duration = d.get_length()
                actual_duration = self._duration
                if actual_duration != nominal_duration:
                    # actual is likely already a Fraction, but just in case:
                    d.factor = Fraction(actual_duration) / nominal_duration
            return d
        else:
            if self._duration > 0:
                return Duration.from_fraction(self._duration)
            else:
                self.message(
                    _("Encountered note at %s without type and duration (=%s)")
                    % (self._when, self._duration))
                return None

    def initialize_pitched_event(self, note_color=None, note_font_size=None):
        pitch = self['pitch'].to_lily_object()
        event = musicexp.NoteEvent()
        event.pitch = pitch

        # <accidental> is optional, but profiling showed that is slightly
        # better to treat it as expected.
        try:
            acc = self['accidental']

            cautionary = getattr(acc, 'cautionary', None)
            editorial = getattr(acc, 'editorial', None)
            parentheses = getattr(acc, 'parentheses', None)
            bracket = getattr(acc, 'bracket', None)

            event.accidental_value = acc.get_text()

            if cautionary == 'yes':
                # According to Gould's book *Behind Bars*, a cautionary
                # accidental can be
                #
                # 1. an ordinary accidental,
                # 2. a parenthesized accidental, or
                # 3. an accidental above the note.
                #
                # Here, we take care of items 1 and 2.
                #
                # TODO: At the time of this writing (April 2024), handling a
                #       combination of `cautionary="yes"` and
                #       `parentheses="no"` is still under discussion for the
                #       forthcoming MusicXML standard version 4.1.
                if parentheses == 'no':
                    event.forced_accidental = True
                else:
                    event.cautionary = True
            if editorial == 'yes':
                if bracket != 'no':
                    event.editorial = True
            if parentheses == 'yes':
                event.cautionary = True
            if bracket == 'yes':
                event.editorial = True
        except KeyError:
            acc = []

        event.accidental_color = getattr(acc, 'color', note_color)
        event.accidental_font_size = getattr(acc, 'font-size', note_font_size)

        # Since `<harmonic>` can change the shape of a note head we have
        # to do an early pass here to set some values.
        for notation in self.get_typed_children(Notations):
            for technical in notation.get_named_children('technical'):
                for harmonic in technical.get_named_children('harmonic'):
                    if not isinstance(event, musicexp.HarmonicNoteEvent):
                        event.__class__ = musicexp.HarmonicNoteEvent
                        event.init()

                    if harmonic.get_named_child('natural'):
                        event.harmonic = 'natural'
                    elif harmonic.get_named_child('artificial'):
                        event.harmonic = 'artificial'
                    else:
                        event.harmonic = 'yes'

                    if harmonic.get_named_child('base-pitch'):
                        event.harmonic_type = 'base-pitch'
                    elif harmonic.get_named_child('touching-pitch'):
                        event.harmonic_type = 'touching-pitch'
                    elif harmonic.get_named_child('sounding-pitch'):
                        event.harmonic_type = 'sounding-pitch'

                    # These attributes are used only for the circular
                    # harmonic symbol.
                    event.harmonic_visible = (getattr(harmonic,
                                                      'print-object',
                                                      'yes') == 'yes')
                    event.harmonic_color = getattr(harmonic, 'color',
                                                   note_color)
                    event.harmonic_font_size = getattr(harmonic, 'font-size',
                                                       note_font_size)

        return event

    def initialize_unpitched_event(self, clef):
        # Unpitched elements can also have `<display-step>` and
        # `<display-octave>` elements.
        event = musicexp.NoteEvent()
        event.pitch = self['unpitched'].to_lily_object(clef)
        return event

    def initialize_rest_event(self, note_color=None, note_font_size=None,
                              convert_rest_positions=True):
        # Rests can have `<display-octave>` and `<display-step>`, which are
        # treated like an ordinary note pitch.
        event = musicexp.RestEvent()
        event.color = getattr(self, 'color', note_color)
        event.font_size = getattr(self, 'font-size', note_font_size)

        if convert_rest_positions:
            pitch = self['rest'].to_lily_object()
            event.pitch = pitch
        return event

    def to_lily_object(self, clef,
                       convert_stem_directions, convert_rest_positions):
        color = getattr(self, 'color', None)
        font_size = getattr(self, 'font-size', None)

        is_rest = False
        if 'pitch' in self:
            event = self.initialize_pitched_event(color, font_size)
        elif 'unpitched' in self:
            event = self.initialize_unpitched_event(clef)
        elif 'rest' in self:
            event = self.initialize_rest_event(color, font_size,
                                               convert_rest_positions)
            is_rest = True
        else:
            self.message(_("cannot find suitable event"))
            return None

        event.duration = self.initialize_duration()

        # LilyPond handles all dots together; we thus only use the first
        # dot's attributes.
        dot = self['dot']
        if dot:
            event.dot_color = getattr(dot[0], 'color', color)
            event.dot_font_size = getattr(dot[0], 'font-size', font_size)

        if not is_rest:
            # Technically, rests can have a `<notehead>` element.  However,
            # this doesn't make any sense...
            notehead = self.get('notehead')
            if (notehead is None
                    and (color is not None or font_size is not None)):
                notehead = Notehead()
            if notehead is not None:
                notehead._duration = self._duration
                for v in notehead.to_lily_object(color, font_size):
                    event.add_associated_event(v)

        stem = self.get('stem')
        if stem is None and color is not None:
            stem = Stem()
        if stem is not None:
            v = stem.to_stem_event(color, is_rest, convert_stem_directions)
            if v is not None:
                event.add_associated_event(v)

        event.visible = (getattr(self, 'print-object', 'yes') == 'yes')
        event.spacing = (getattr(self, 'print-spacing', 'yes') == 'yes')

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
        self.senza_misura_length = 0

    def is_implicit(self):
        return getattr(self, 'implicit', None) == 'yes'

    def get_notes(self):
        return self.get_typed_children(get_class('note'))


class Syllabic(Music_xml_node):
    def continued(self):
        text = self.get_text()
        return text == "begin" or text == "middle"


class Lyric(Music_xml_node):
    pass


class Sound(Music_xml_node):
    def get_tempo(self):
        """
        Return the tempo attribute(if it exists) of the sound element.
        This attribute can be used by musicxml2ly for the midi output
        (see L{musicexp.Score}).

        @rtype: string
        @return: The value of the tempo attribute
        """
        return getattr(self, 'tempo', None)


class Notations(Music_xml_node):
    max_occurs_by_child = {
        'arpeggiate': 2,
        'non-arpeggiate': 2,
        'slur': 2,
    }

    def __init__(self):
        Music_xml_node.__init__(self)
        self._content['arpeggiate'] = []
        self._content['non-arpeggiate'] = []
        self._content['slur'] = []

    def get_tie(self):
        # TODO: Support ties starting and stopping in the same `<notations>`
        #       element (thus becoming either `\laissezVibrer` or
        #       `\repeatTie`).
        # TODO: Support `let-ring` attribute (becoming `\laissezVibrer`).
        ts = self.get_named_children('tied')
        starts = [t for t in ts if t.type == 'start']
        if starts:
            return starts[0]
        else:
            return None

    def get_tuplets(self):
        return self.get_typed_children(Tuplet)


class Time(Music_xml_node):
    def __init__(self):
        Music_xml_node.__init__(self)
        self.senza_misura_length = 0


class Time_modification(Music_xml_node):
    def get_fraction(self):
        b = self.get_maybe_exist_named_child('actual-notes')
        a = self.get_maybe_exist_named_child('normal-notes')
        return (int(a.get_text()), int(b.get_text()))

    def get_normal_type(self):
        tuplet_type = self.get_maybe_exist_named_child('normal-type')
        if tuplet_type:
            dots = self.get_named_children('normal-dot')
            log = utilities.musicxml_duration_to_log(
                tuplet_type.get_text().strip())
            return (log, len(dots))
        else:
            return None


class Accidental(Music_xml_node):
    pass


class Tuplet(Music_xml_spanner):
    def duration_info_from_tuplet_note(self, tuplet_note):
        tuplet_type = tuplet_note.get_maybe_exist_named_child('tuplet-type')
        if tuplet_type:
            dots = tuplet_note.get_named_children('tuplet-dot')
            log = utilities.musicxml_duration_to_log(
                tuplet_type.get_text().strip())
            return (log, len(dots))
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
        return self.get_tuplet_note_count(
            self.get_maybe_exist_named_child('tuplet-normal'))

    def get_actual_nr(self):
        return self.get_tuplet_note_count(
            self.get_maybe_exist_named_child('tuplet-actual'))

    # It is only possible to modify the appearance of the triplet number but
    # not the appearance of the triplet bracket (see
    # https://github.com/w3c/musicxml/issues/538 for more).  We only look at
    # the `<tuplet-number>` child of the `<tuplet-actual>` element.
    def get_tuplet_number_attributes(self):
        color = None
        font_size = None

        tuplet_actual = self.get_maybe_exist_named_child('tuplet-actual')
        if tuplet_actual:
            tuplet_number = tuplet_actual.get_maybe_exist_named_child(
                'tuplet-number')
            if tuplet_number:
                color = getattr(tuplet_number, 'color', None)
                font_size = getattr(tuplet_number, 'font-size', None)

        return (color, font_size)


class Slur(Music_xml_spanner):
    pass


class Tied(Music_xml_spanner):
    pass


class Beam(Music_xml_spanner):
    def get_type(self):
        return self.get_text()  # <beam> has no 'type' attribute

    def is_primary(self):
        return getattr(self, 'number', '1') == '1'


class Octave_shift(Music_xml_spanner):
    size = '8'


# Rests in MusicXML are <note> blocks with a <rest> inside. This class is only
# for the inner <rest> element, not the whole rest block.
class Rest(Music_xml_node):
    max_occurs_by_child = {
        'display-octave': 1,
        'display-step': 1,
    }

    def __init__(self):
        Music_xml_node.__init__(self)
        self.single_voice = None  # Not set for invisible rests.
        self._is_whole_measure = False

    def is_whole_measure(self):
        return self._is_whole_measure

    def to_lily_object(self):
        p = None
        step = self.get('display-step')
        if step:
            p = musicexp.Pitch()
            p.step = conversion.musicxml_step_to_lily(step)
            # if display-step is present, display-octave must be present too
            p.octave = self['display-octave'] - 4
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
        return {'add': 1,
                'alter': 1,
                'subtract': -1}.get(ch.get_text().strip(), 0)

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
        if isinstance(e, Note):
            try:
                name = e['staff']
                if not self._start_staff and ('grace' not in e):
                    self._start_staff = name
                self._staves[name] = True
            except KeyError:  # no <staff>
                pass

            lyrics = e['lyric']
            if not self._has_lyrics:
                self.has_lyrics = len(lyrics) > 0

            for l in lyrics:
                nr = getattr(l, 'number', None)
                if nr is not None and nr not in self._lyrics:
                    self._lyrics.append(nr)

    def insert(self, idx, e):
        self._elements.insert(idx, e)

    def get_lyrics_numbers(self):
        if len(self._lyrics) == 0 and self._has_lyrics:
            # only happens if none of the <lyric> tags has a number attribute
            return ['1']
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

    # Set durations and starting points of all notes and measures.  Note
    # that the starting point of the very first note is 0.
    def interpret(self):
        part_list = self.get_part_list()

        now = 0
        factor = 1
        attributes_dict = {}
        attributes_object = None
        measures = self.get_typed_children(Measure)
        last_moment = -1
        last_measure_position = -1
        measure_position = 0
        measure_start_moment = now
        is_first_measure = True
        previous_measure = None

        # Graces at the end of a measure need to have their position set to
        # the previous moment.
        pending_graces = []

        # In 'senza misura' mode we have to use actual note and rest lengths
        # to insert (hidden) time signatures so that bar checks for LilyPond
        # work properly.  For both measures and 'senza misura' time
        # signatures, `senza_misura_length` holds the distance to the next
        # time signature (or end of measure).
        senza_misura_mode = False
        senza_misura_in_previous_measure = False
        senza_misura_moment = 0
        previous_senza_misura = None  # `Measure` or `Time_modification`

        for m in measures:
            if senza_misura_mode:
                if previous_senza_misura:
                    previous_senza_misura.senza_misura_length = \
                        now - senza_misura_moment
                    senza_misura_moment = now
                previous_senza_misura = m
                senza_misura_in_previous_measure = True

            # Implicit measures are used for artificial measures, for
            # example, when a repeat bar line splits a bar into two halves.
            # In this case, don't reset the measure position to 0.
            #
            # They are also used for upbeats (initial value of 0 fits these,
            # too).  Also, don't reset the measure position at the end of
            # the loop, but rather when starting the next measure (since
            # only then we know whether the next measure is implicit and
            # continues that measure).
            if not m.is_implicit():
                # Warn about possibly incomplete or overfull measures and
                # reset the position.
                if (attributes_object
                        and previous_measure
                        and previous_measure.partial == 0):
                    if not senza_misura_in_previous_measure:
                        length = attributes_object.get_measure_length()
                        new_now = measure_start_moment + length
                        if now != new_now:
                            problem = _('Incomplete')
                            if now > new_now:
                                problem = _('Overfull')
                            # Only for verbose operation.
                            if problem != _('Incomplete') and previous_measure:
                                previous_measure.message(
                                    _('%s measure? '
                                      'Expected: %s, difference: %s')
                                    % (problem, now, new_now - now))
                        now = new_now

                measure_start_moment = now
                measure_position = 0
                senza_misura_in_previous_measure = False

            voice_id = None
            assign_to_next_voice = []
            grace_length = 0
            last_voice_id = None

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

                # Figured bass has a duration but it applies to the next note
                # and should not change the current measure position!
                if isinstance(n, FiguredBass):
                    n._divisions = factor.denominator
                    n._when = now
                    n._measure_position = measure_position
                    continue

                if isinstance(n, Hash_text):
                    continue

                dur = 0

                if n.__class__ == Attributes:
                    n.set_attributes_from_previous(attributes_dict)
                    n.read_self()
                    attributes_dict = n._dict.copy()
                    attributes_object = n

                    # default to <divisions>1</divisions>
                    divisions = (int(attributes_dict['divisions'].get_text())
                                 if 'divisions' in attributes_dict else 1)
                    factor = Fraction(1, divisions)

                    if 'time' in attributes_dict:
                        if senza_misura_mode and previous_senza_misura:
                            previous_senza_misura.senza_misura_length = \
                                now - senza_misura_moment
                            senza_misura_moment = now

                        if attributes_object.get_measure_length() < 0:
                            senza_misura_mode = True
                            senza_misura_in_previous_measure = True
                            previous_senza_misura = attributes_dict['time']
                        else:
                            senza_misura_mode = False
                            previous_senza_misura = None

                if 'duration' in n:
                    dur = Fraction(n['duration'], 4) * factor

                    if n.get_name() == 'backup':
                        dur = -dur
                        # Change all graces before the backup to after-graces.
                        self.graces_to_aftergraces(pending_graces)
                        pending_graces = []
                    if 'grace' in n:  # not expected to coexist with 'duration'
                        dur = 0

                    rest = n.get('rest')
                    if (rest
                            and attributes_object
                            and attributes_object.get_measure_length() == dur):
                        rest._is_whole_measure = True

                # Use main note duration for chord notes.
                if dur > 0 and 'chord' in n:
                    now = last_moment
                    measure_position = last_measure_position

                n._when = now
                n._measure_position = measure_position

                if last_voice_id != voice_id or now > 0:
                    if grace_length > 0:
                        starting_grace_lengths[(self.id,
                                                last_voice_id)] = grace_length
                        grace_length = 0
                    last_voice_id = voice_id

                if 'grace' in n:
                    if now == 0 and 'chord' not in n:
                        # TODO: Handle other situations, too, where grace
                        #       synchronization is necessary.
                        from musicexp import Duration
                        duration_info = n.get_duration_info()
                        d = Duration()
                        d.duration_log = duration_info[0]
                        d.dots = duration_info[1]

                        grace_length += d.get_length()

                    # For all grace notes, store the previous note in case
                    # we need to turn the grace note into an after-grace
                    # later on.
                    n._prev_when = last_moment
                    n._prev_measure_position = last_measure_position
                    # After-graces are placed at the same position as the
                    # previous note.
                    if n.is_after_grace():
                        # TODO: We should do the same for grace notes at the
                        #       end of a measure with no following note!
                        n._when = last_moment
                        n._measure_position = last_measure_position
                    else:
                        pending_graces.append(n)
                elif dur > 0:
                    pending_graces = []

                n._duration = dur
                if dur > 0:
                    last_moment = now
                    last_measure_position = measure_position
                    now += dur
                    measure_position += dur
                elif dur < 0:
                    # backup element, reset measure position
                    now += dur
                    measure_position += dur
                    if measure_position < 0:
                        # backup went beyond the measure start => reset to 0
                        now -= measure_position
                        measure_position = 0
                    last_moment = now
                    last_measure_position = measure_position

                instruments = n.get('instrument', [])  # only in <note>
                if len(instruments) > 0:
                    # TODO: <note> can contain any number of <instrument> but
                    # we are only paying attention to the first.
                    instr = instruments[0]
                    n.instrument_name = part_list.get_instrument(instr.id)

            if grace_length > 0:
                starting_grace_lengths[(self.id, voice_id)] = grace_length

            # Change all graces at the end of the measure to after-graces.
            self.graces_to_aftergraces(pending_graces)
            pending_graces = []

            # Incomplete first measures are not padded but registered as
            # partial.
            if is_first_measure:
                is_first_measure = False
                # upbeats are marked as implicit measures
                if attributes_object and m.is_implicit():
                    length = attributes_object.get_measure_length()
                    measure_end = measure_start_moment + length
                    if measure_end != now:
                        m.partial = now
            previous_measure = m

        if senza_misura_mode and previous_senza_misura:
            previous_senza_misura.senza_misura_length = \
                now - senza_misura_moment

    def add_note(self, intervals, note):
        start = note._when
        end = start + note._duration

        new_count = 0

        # `start` and `end` span an interval.  Either create the union of
        # this interval with existing ones or insert it if there is no
        # overlap.
        node = intervals.first
        while node is not None:
            (left, right, count) = node.value

            if start < left and end <= left:
                # New interval is left of current interval.
                intervals.insert([start, end, 0], node)
                return
            if start >= right and end > right:
                # New interval is right of current interval.
                node = node.next
                continue
            if start >= left and end <= right:
                # New interval is enclosed in current interval.
                node.value[2] = 1
                return
            if start <= left and end >= right:
                # New interval encloses current interval.
                curr_node = node
                node = node.next
                intervals.remove(curr_node)
                new_count = 1
                continue
            if start < left and end <= right:
                # New interval is left of current interval and overlaps.
                node.value[0] = start
                node.value[2] = 1
                return
            if start >= left and end > right:
                # New interval is right of current interval and overlaps.
                node.value[1] = end
                node.value[2] = 1
                return

        intervals.append([start, end, new_count])

    # If `note` is in an interval with its counter equal to zero, we have a
    # hit.
    def is_single_voice(self, intervals, note):
        pos = note._when

        node = intervals.first
        while node is not None:
            # All intervals are non-intersecting, and `note` is certainly in
            # one of the `intervals` elements.  It is thus sufficient to
            # find the first interval end larger than `pos`.
            if pos < node.value[1]:
                return node.value[2] == 0
            node = node.next

    # Walk over all measures and mark `<note>` objects that are 'single
    # voice', i.e., no other voice (in a particular staff) produces visible
    # output while such an object is active.
    def tag_single_voices(self):
        measures = self.get_typed_children(Measure)
        for m in measures:
            intervals = {}

            # Use all notes of the measure to find overlapping intervals,...
            for n in m.get_all_children():
                if (isinstance(n, Note)
                        and getattr(n, 'print-object', 'yes') == 'yes'
                        and 'chord' not in n):
                    sid = n.get('staff', 'None')
                    if sid not in intervals:
                        intervals[sid] = dllist()
                    self.add_note(intervals[sid], n)

            # ... then pass the result back to the notes.
            for n in m.get_all_children():
                if (isinstance(n, Note)
                        and getattr(n, 'print-object', 'yes') == 'yes'
                        and 'chord' not in n):
                    sid = n.get('staff', 'None')
                    n.single_voice = self.is_single_voice(intervals[sid], n)

    # modify attributes so that only those applying to the given staff remain
    def extract_attributes_for_staff(part, attr, staff):
        attributes = copy.copy(attr)
        attributes._children = []
        attributes._dict = attr._dict.copy()
        attributes._original_tag = attr
        # copy only the relevant children over for the given staff
        if staff is None:
            staff = "1"
        for c in attr._children:
            if (getattr(c, 'number', staff) == staff
                    and not isinstance(c, Hash_text)):
                attributes._children.append(c)
        if not attributes._children:
            return None
        else:
            return attributes

    def extract_voices(part):
        # LilyPond needs to know properties of the end of some spanners
        # before the spanner code gets actually emitted (and sometimes vice
        # versa).  To enable that we set links between a spanner's start
        # part and its end part.
        #
        # Similarly, middle parts of spanners sometimes need to know details
        # of the start spanner element (for example, accidental marks
        # attached to a middle part of a wavy line need to know the
        # `placement` attribute of the start part).
        def link_spanners(elements, structure, type, one_child=True):
            spanner_starts = {}

            for e in elements:
                link_spanners_(e, structure, type, one_child, spanner_starts)

        def link_spanners_(element, structure, type, one_child,
                           spanner_starts):
            if not isinstance(element, structure[0]):
                return

            if len(structure) > 1:
                for tc in element.get_typed_children(structure[1]):
                    link_spanners_(tc, structure[1:], type, one_child,
                                   spanner_starts)
            else:
                if one_child:
                    spanners = element.get_named_child(type)
                    if spanners is not None:
                        spanners = [spanners]
                else:
                    spanners = element.get_named_children(type)
                if spanners is None:
                    return

                for spanner in spanners:
                    nr = getattr(spanner, 'number', '1')
                    if spanner.type == 'start':
                        spanner_starts[nr] = spanner
                    elif spanner.type == 'continue':
                        if nr in spanner_starts:
                            spanner.paired_with = spanner_starts[nr]
                    elif spanner.type == 'stop':
                        if nr in spanner_starts:
                            spanner_starts[nr].paired_with = spanner
                            spanner.paired_with = spanner_starts[nr]
                            del spanner_starts[nr]
                        else:
                            warnings.warn(_('%s end seen without %s start'
                                            % (type, type)))

        measures = part.get_typed_children(Measure)
        elements = []
        for m in measures:
            elements.append(m)
            if m.partial > 0:
                elements.append(Partial(m.partial))
            elements.extend(m.get_all_children())

        link_spanners(elements, [Direction, DirType], 'bracket')
        link_spanners(elements, [Direction, DirType], 'dashes')
        link_spanners(elements, [Note, Notations, Ornaments], 'wavy-line',
                      one_child=False)

        voices = OrderedDict()
        last_voice = None
        voice_to_staff_dict = {}

        # We now do a pre-pass to collect all voice IDs so that dynamics,
        # clefs, etc., can be assigned to the correct voices.
        for n in elements:
            try:
                vid = n['voice']
            except KeyError:
                if isinstance(n, Note):
                    vid = last_voice if 'chord' in n else "1"
                else:
                    vid = None

            if vid is not None:
                last_voice = vid

            sid = n.get('staff', None)
            if vid is not None and vid not in voices:
                voices[vid] = Musicxml_voice()
            if vid is not None and sid is not None and 'grace' not in n:
                if vid not in voice_to_staff_dict:
                    voice_to_staff_dict[vid] = sid

        # We need at least one voice and one staff ID.
        if not voices:
            voices['1'] = Musicxml_voice()
        if not voice_to_staff_dict:
            voice_to_staff_dict = {v: '1' for v in voices}

        # Invert the `voice_to_staff_dict` into a `staff_to_voice_dict`
        # (since we need to assign staff-related objects like clefs, times,
        # etc., to the correct voices).  This will never be entirely correct
        # due to staff switches, but it is the best we can do.
        staff_to_voice_dict = {}
        for (v, s) in voice_to_staff_dict.items():
            if s not in staff_to_voice_dict:
                staff_to_voice_dict[s] = [v]
            else:
                staff_to_voice_dict[s].append(v)

        start_attr = None
        assign_to_next_note = []
        id = None

        def prev_curr_iter(elems):
            prev_e = None
            for e in elems:
                yield (prev_e, e)
                if not isinstance(e, Hash_text):
                    prev_e = e

        for (prev_n, n) in prev_curr_iter(elements):
            try:
                id = n['voice']
            except KeyError:
                if isinstance(n, Note):
                    id = last_voice if 'chord' in n else '1'

            if id is not None:
                last_voice = id

            # We don't need `<backup>` and `<forward>` any more since we
            # have already assigned the correct onset times.
            #
            # TODO: Pass `<grouping>`, `<link>`, `<bookmark>`, `<sound>`.
            if not isinstance(n, (Note,
                                  Attributes,
                                  Direction,
                                  Measure,
                                  Partial,
                                  Barline,
                                  Harmony,
                                  FiguredBass,
                                  Print)):
                continue

            if isinstance(n, Attributes) and not start_attr:
                start_attr = n
                continue

            if isinstance(n, Attributes):
                # assign these only to the voices they really belong to!
                for (s, vids) in staff_to_voice_dict.items():
                    staff_attributes = part.extract_attributes_for_staff(n, s)
                    if staff_attributes:
                        for v in vids:
                            voices[v].add_element(staff_attributes)
                continue

            if isinstance(n, (Measure,
                              Partial,
                              Barline)):
                if id is None:
                    id = '1'
                for i in assign_to_next_note:
                    voices[id].add_element(i)
                assign_to_next_note = []

                for v in voices.keys():
                    voices[v].add_element(n)
                continue

            if isinstance(n, Print):
                for v in voices.keys():
                    voices[v].add_element(n)
                continue

            if isinstance(n, Direction):
                if isinstance(prev_n, Direction):
                    # If there are two `<direction>` elements 'A' and 'B' in
                    # succession we do some heuristic checks to find out
                    # whether they are connected and can be 'chained' and
                    # concatenated later on.
                    #
                    # (1) The first `<direction-type>` child of 'A' and
                    #     'B' have both approximately the same `default-y`
                    #     value.  We don't inspect other children for
                    #     simplicity.
                    #
                    # (2) Neither 'A' nor 'B' has a `<staff>` child, or both
                    #     'A' and 'B' have a `<staff>` child with the same
                    #     value.
                    #
                    # (3) The `<offset>` child in 'A' and 'B' have different
                    #     values (a missing `<offset>` child is interpreted
                    #     as value zero).
                    #
                    # Example: `<words>` in 'A' contains 'Allegro' and
                    # `<metronome>` in 'B' contains '<quarter> = 120'.
                    #
                    # TODO: The above algorithm could be generalized to
                    #       chain more than two successive `<direction>`
                    #       elements; investigate whether this happens in
                    #       real-world scores.
                    #
                    # TODO: Properly handle successive `<direction>`
                    #       elements at different vertical positions.  Right
                    #       now this causes LilyPond warnings (and the
                    #       omission of events) since it doesn't support
                    #       multiple occurrences of `\mark` or `\tempo` at a
                    #       given musical moment.
                    prev_dirtype = \
                        next(c for c in prev_n.get_typed_children(DirType))
                    prev_first_child = \
                        next(c for c in prev_dirtype.get_all_children()
                             if not isinstance(c, Hash_text))
                    prev_default_y = \
                        getattr(prev_first_child, 'default-y', None)

                    dirtype = next(c for c in n.get_typed_children(DirType))
                    first_child = next(c for c in dirtype.get_all_children()
                                       if not isinstance(c, Hash_text))
                    default_y = getattr(first_child, 'default-y', None)

                    # Condition (1).  We use half an interline staff space
                    # as a heuristic threshold.
                    if (prev_default_y is not None
                            and default_y is not None
                            and abs(float(prev_default_y)
                                    - float(default_y)) < 5):
                        prev_staff = prev_n.get('staff', '0')
                        staff = n.get('staff', '0')

                        # Condition (2).
                        if prev_staff == staff:
                            prev_offset_elem = prev_n.get_named_child('offset')
                            prev_offset = 0
                            if prev_offset_elem is not None:
                                prev_offset = float(
                                    prev_offset_elem.get_text())

                            offset_elem = n.get_named_child('offset')
                            offset = 0
                            if offset_elem is not None:
                                offset = float(offset_elem.get_text())

                            # Condition (3).
                            if prev_offset == offset:
                                n.message(_('Found overlapping '
                                            '<direction> elements'))
                                offset += 1  # Arbitrary choice.

                            if prev_offset > offset:
                                n.next = prev_n
                                prev_n.prev = n
                            else:
                                prev_n.next = n
                                n.prev = prev_n

                if n.voice_id:
                    voices[n.voice_id].add_element(n)
                else:
                    assign_to_next_note.append(n)
                continue

            if isinstance(n, (Harmony, FiguredBass)):
                # store the harmony or figured bass element until we encounter
                # the next note and assign it only to that one voice.
                assign_to_next_note.append(n)
                continue

            # At this point, `n` is a `<note>` element.
            for i in assign_to_next_note:
                voices[id].add_element(i)
            assign_to_next_note = []
            voices[id].add_element(n)

        # Assign all remaining elements from `assign_to_next_note` to the
        # voice of the previous note (if any).
        if id is None:
            id = '1'
        for i in assign_to_next_note:
            voices[id].add_element(i)

        # Insert start attributes into all staves of the current part.
        if start_attr:
            for (s, vids) in staff_to_voice_dict.items():
                staff_attributes = part.extract_attributes_for_staff(
                    start_attr, s)
                staff_attributes.read_self()
                part._staff_attributes_dict[s] = staff_attributes
                for v in vids:
                    # Element 0 is a `Measure` object.
                    voices[v].insert(1, staff_attributes)
                    voices[v]._elements[1].read_self()

        part._voices = voices

    def get_voices(self):
        return self._voices

    def get_staff_attributes(self):
        return self._staff_attributes_dict


class BarStyle(Music_xml_node):
    pass


class Metronome(Music_xml_node):
    pass


class BeatType(Music_xml_node):
    pass


class BeatUnit(Music_xml_node):
    pass


class BeatUnitDot(Music_xml_node):
    pass


class BeatUnitTied(Music_xml_node):
    pass


class Beats(Music_xml_node):
    pass


class Bracket(Music_xml_spanner):
    pass


class Chord(Music_xml_node):
    minidom_demarshal_to_value = minidom_demarshal_true


class Credit_words(Xml_node):
    pass


class Credit_symbol(Xml_node):
    pass


class Dashes(Music_xml_spanner):
    pass


class DirType(Music_xml_node):
    pass


class Direction(Measure_element):
    max_occurs_by_child = {
        'staff': 1,
        'voice': 1,
    }

    def __init__(self):
        Measure_element.__init__(self)
        # For chaining with another `<direction>` element.
        self.prev = None
        self.next = None

    # We assume there is only a single pedal spanner in the `<Direction>`
    # element.  Additionally, we only consider the `line` attribute at the
    # beginning, assuming that the remaining parts of the pedal spanner are
    # of the same type.
    def pedal_is_line(self):
        for dt in self.get_typed_children(DirType):
            pedal = dt.get_named_child('pedal')
            if pedal:
                break

        if pedal and pedal.type == 'start':
            return (getattr(pedal, 'line', 'no') == 'yes')
        else:
            return None


class Elision(Music_xml_node):
    pass


class Extend(Music_xml_node):
    pass


class FiguredBass(Music_xml_node):
    max_occurs_by_child = {
        'duration': 1,
    }


class Forward(Measure_element):
    max_occurs_by_child = {
        'duration': 1,
        'staff': 1,
        'voice': 1,
    }


class Glissando(Music_xml_spanner):
    pass


class Grace(Music_xml_node):
    """optional empty child of <note>"""
    pass


class Harmony(Music_xml_node):
    max_occurs_by_child = {
        'staff': 1,
    }


class Hash_comment(Music_xml_node):
    pass


class KeyAlter(Music_xml_node):
    pass


class KeyOctave(Music_xml_node):
    pass


class KeyStep(Music_xml_node):
    pass


class Octave(Music_xml_node):
    minidom_demarshal_to_value = minidom_demarshal_text_to_int  # 0 to 9, per the spec


class Display_octave(Octave):
    pass


class Ornaments(Music_xml_node):
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
    # TODO: We are leaving this staff number as text when we could convert it to
    # an integer.  It would make more sense to convert it to an integer.  The
    # spec says that the staves of a part are numbered in order starting from 1
    # at the top.  We could track them in lists rather than in dictionaries.
    # Other staff-number attributes (e.g., the 'number' attribute of <clef>)
    # would need to be converted too.
    minidom_demarshal_to_value = minidom_demarshal_text_to_str


class Step(Music_xml_node):
    minidom_demarshal_to_value = minidom_demarshal_text_to_str


class Display_step(Step):
    pass


class Text(Music_xml_node):
    pass


class Type(Music_xml_node):
    pass


class Voice(Music_xml_node):
    minidom_demarshal_to_value = minidom_demarshal_text_to_str


class Wavy_line(Music_xml_spanner):
    def __init__(self):
        Music_xml_spanner.__init__(self)
        self.start_stop = False


class Wedge(Music_xml_spanner):
    pass


class Words(Music_xml_node):
    pass


# A tremolo can be either a normal ornamentation (single-note tremolo) or a
# spanner (double-note tremolo).
class Tremolo(Music_xml_spanner):
    # The `type` attribute is optional, defaulting to `single`.
    def get_type(self):
        return self._attribute_dict.get('type', 'single')


# We need this since not all classes are instantiated for every input file.
# Only add classes that are either directly used by class name or extend
# `Music_xml_node` in some way!
class_dict = {
    '#comment': Hash_comment,
    '#text': Hash_text,
    'accidental': Accidental,
    'alter': Alter,
    'arpeggiate': Arpeggiate,
    'attributes': Attributes,
    'backup': Backup,
    'barline': Barline,
    'bar-style': BarStyle,
    'bass': Bass,
    'beam': Beam,
    'beats': Beats,
    'beat-type': BeatType,
    'beat-unit': BeatUnit,
    'beat-unit-dot': BeatUnitDot,
    'beat-unit-tied': BeatUnitTied,
    'bend': Bend,
    'bracket': Bracket,
    'chord': Chord,
    'credit': Credit,
    'credit-words': Credit_words,
    'credit-symbol': Credit_symbol,
    'dashes': Dashes,
    'degree': ChordModification,
    'direction': Direction,
    'direction-type': DirType,
    'display-octave': Display_octave,
    'display-step': Display_step,
    'duration': Duration,
    'elision': Elision,
    'extend': Extend,
    'forward': Forward,
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
    'metronome': Metronome,
    'non-arpeggiate': NonArpeggiate,
    'notations': Notations,
    'note': Note,
    'notehead': Notehead,
    'octave': Octave,
    'octave-shift': Octave_shift,
    'ornaments': Ornaments,
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
    'step': Step,
    'syllabic': Syllabic,
    'text': Text,
    'time': Time,
    'time-modification': Time_modification,
    'tied': Tied,
    'tremolo': Tremolo,
    'tuplet': Tuplet,
    'type': Type,
    'unpitched': Unpitched,
    'voice': Voice,
    'wavy-line': Wavy_line,
    'wedge': Wedge,
    'words': Words,
    'work': Work,
}


for name, cls in class_dict.items():
    cls._name = name


def name2class_name(name):
    name = name.replace('-', '_')
    name = name.replace('#', 'hash_')
    name = name[0].upper() + name[1:].lower()

    return str(name)


def get_class(name):
    try:
        return class_dict[name]
    except KeyError:
        pass
    class_name = name2class_name(name)
    klass = type(class_name, (Music_xml_node,), {'_name': name})
    class_dict[name] = klass
    return klass


def minidom_demarshal_node(node, py_parent=None):
    name = node.nodeName
    cls = get_class(name)

    # For certain leaf elements of the schema, instead of creating a full child
    # node, we just create a value.
    try:
        value = cls.minidom_demarshal_to_value(node)
        # TODO: Create lists when `max_occurs_by_child` > 1?
        assert type(py_parent).max_occurs_by_child[name] == 1
        py_parent._content[name] = value
        return
    except AttributeError:  # minidom_demarshal_to_value not in cls
        pass

    # Create a node
    py_node = cls()
    py_node._parent = py_parent
    py_node._children = [minidom_demarshal_node(cn, py_node)
                         for cn in node.childNodes]

    attributes = node.attributes
    if attributes:
        for nm, value in attributes.items():
            py_node.__dict__[nm] = value
            py_node._attribute_dict[nm] = value

    if node.nodeType == node.TEXT_NODE:
        py_node._data = node.data

    try:
        max_occurs = type(py_parent).max_occurs_by_child[name]
    except (AttributeError, KeyError):
        max_occurs = None

    if max_occurs == 1:
        py_parent._content[name] = py_node
    elif max_occurs == 2:  # unlimited
        # The parent's constructor is required to initialize an empty list.
        py_parent._content[name].append(py_node)

    return py_node
