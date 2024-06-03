# musicexp.py
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


from fractions import Fraction
import inspect
import math
import re
import sys
import utilities
import warnings

import lilylib as ly

# Store previously converted pitch for \relative conversion as a global
# state variable
previous_pitch = None
relative_pitches = False
whatOrnament = ""
ly_dur = None  # For communication between `Duration` and `TremoloEvent`.


def escape_instrument_string(input_string):
    retstring = input_string.replace('"', r'\"')
    if re.match('.*[\r\n]+.*', retstring):
        rx = re.compile(r'[\n\r]+')
        strings = rx.split(retstring)
        retstring = r"\markup { \center-column { "
        for s in strings:
            retstring += r'\line { "' + s + '" } '
        retstring += "} }"
    else:
        retstring = '"' + retstring + '"'
    return retstring


class Output_stack_element:
    def __init__(self):
        self.factor = 1  # For scaling tuplets and the like.

    def copy(self):
        o = Output_stack_element()
        o.factor = self.factor
        return o


class Output_printer(object):
    """
    A class that takes care of formatting (eg.: indenting) a
    Music expression as a .ly file.
    """

    def __init__(self):
        self._line = ''
        self._indent = 2
        self._nesting = 0
        self._nesting_str = ''
        self._nesting_more_str = ''
        self._nesting_less_str = ''
        self._file = sys.stdout
        self._line_len = 80
        self._output_state_stack = [Output_stack_element()]
        self._skipspace = False
        self._last_duration = None

    def set_file(self, file):
        self._file = file

    def dump_version(self, version):
        self.print_verbatim(r'\version "' + version + '"')
        self.newline()

    def get_indent(self):
        return self._nesting * self._indent

    def override(self):
        last = self._output_state_stack[-1]
        self._output_state_stack.append(last.copy())

    def add_factor(self, factor):
        self.override()
        self._output_state_stack[-1].factor *= factor

    def revert(self):
        del self._output_state_stack[-1]
        if not self._output_state_stack:
            raise RuntimeError('empty stack')

    def duration_factor(self):
        return Fraction(self._output_state_stack[-1].factor)

    def print_verbatim(self, s):
        self._line += s

    def set_nesting_strings(self):
        self._nesting_str = ' ' * self._indent * self._nesting
        self._nesting_more_str = ' ' * self._indent * (self._nesting + 1)
        if self._nesting > 0:
            self._nesting_less_str = ' ' * self._indent * (self._nesting - 1)
        else:
            self._nesting_less_str = ''

    def unformatted_output(self, s):
        # don't indent on \< and indent only once on <<
        self._nesting += (s.count('<')
                          - s.count(r'\<') - s.count('<<')
                          + s.count('{'))
        self._nesting -= (s.count('>') - s.count(r'\>') - s.count('>>')
                          - s.count('->') - s.count('_>')
                          - s.count('^>')
                          + s.count('}'))
        self.set_nesting_strings()
        self.print_verbatim(s)

    def print_duration_string(self, s):
        if self._last_duration == s:
            return

        self.unformatted_output(s)

#    def print_note_color(self, object, rgb=None):
#        if rgb:
#            str = (r"\once\override %s.color = #(rgb-color %s # %s %s)"
#                  % (object, rgb[0], rgb[1], rgb[2]))
#        else:
#            str = r"\revert %s.color" % object
#            self.newline()
#            self.add_word(str)
#            self.newline()

    def add_word(self, s):
        if len(s) + 1 + len(self._line) > self._line_len:
            self.newline()
            self._skipspace = True

        if not self._skipspace:
            self._line += ' '
        self.unformatted_output(s)
        self._skipspace = False

    def newline(self):
        # Correct indentation for `}`, `>>`, and `} <<` on a line by its
        # own.
        to_replace = self._nesting_more_str + r'(>>|})\s*$'
        replace_with = self._nesting_str + r'\1'
        self._line = re.sub(to_replace, replace_with, self._line)
        to_replace = self._nesting_str + r'} <<\s*$'
        replace_with = self._nesting_less_str + '} <<'
        self._line = re.sub(to_replace, replace_with, self._line)

        self._file.write(self._line + '\n')
        self._line = self._nesting_str
        self._skipspace = True

    def skipspace(self):
        self._skipspace = True

    def __call__(self, arg):
        self.dump(arg)

    def dump(self, s):
        if not s:
            return

        if self._skipspace:
            self._skipspace = False
            self.unformatted_output(s)
        else:
            # Avoid splitting quoted strings (e.g. "1. Wie") when indenting.
            words = utilities.split_string_and_preserve_doublequoted_substrings(
                s)
            for w in words:
                self.add_word(w)

    def dump_texidoc(self, s):
        words = utilities.split_string_and_preserve_doublequoted_substrings(s)
        if words:
            words[0] = '"' + words[0]
            words[-1] += '"'

            self._nesting += 1
            self.set_nesting_strings()

            for w in words:
                self.add_word(w)

            self._nesting -= 1
            self.set_nesting_strings()

    def dump_lyrics(self, s):
        words = utilities.split_string_and_preserve_doublequoted_substrings(s)
        for w in words:
            self.add_word(w)
        return

    def close(self):
        self.newline()
        self._file.close()
        self._file = None


class Duration:
    def __init__(self):
        self.duration_log = 0
        self.dots = 0
        self.factor = 1

    @classmethod
    def from_fraction(cls, length):
        """Create a Duration from a Fraction or int"""
        dur = Duration()
        dur.set_from_fraction(length)
        return dur

    def set_from_fraction(self, length):
        """Set this Duration from a Fraction or int"""
        try:  # assume length is a Fraction
            d = length.denominator
            if d > 1:
                dlog = d.bit_length() - 1
                if (1 << dlog) == d:  # d is a power of 2
                    # TODO: Handling n % 3 == 0 (with factor = n // 3) improved
                    # code readability for a real-world sample score with a mix
                    # of 2/4, 3/4, 4/4, 6/4, 6/8, and 12/8 time signatures.
                    n = length.numerator
                    if n == 3:  # e.g., s1. rather than s2*3
                        self.duration_log = dlog - 1
                        self.dots = 1
                        self.factor = 1
                        return
                    else:  # e.g., s8*5
                        self.duration_log = dlog
                        self.dots = 0
                        self.factor = n
                        return
        except AttributeError:  # length is an int (probably)
            pass
        # e.g., s1*length
        self.duration_log = 0
        self.dots = 0
        self.factor = length

    def lisp_expression(self):
        return '(ly:make-duration %d %d %s)' % (self.duration_log,
                                                self.dots,
                                                self.factor)

    def ly_expression(self, factor=None, scheme_mode=False):
        if not factor:
            factor = self.factor

        global ly_dur  # For communication with `TremoloEvent`.
        ly_dur = self.duration_log

        if self.duration_log < 0:
            if scheme_mode:
                longer_dict = {-1: "breve", -2: "longa"}
            else:
                longer_dict = {-1: r"\breve", -2: r"\longa"}
            dur_str = longer_dict.get(self.duration_log, "1")
        else:
            dur_str = '%d' % (1 << self.duration_log)
        dur_str += '.' * self.dots

        if factor != 1:
            dur_str += f'*{factor}'

        return dur_str

    def print_ly(self, outputter):
        dur_str = self.ly_expression(self.factor / outputter.duration_factor())
        outputter.print_duration_string(dur_str)

    def __repr__(self):
        return self.ly_expression()

    def copy(self):
        d = Duration()
        d.dots = self.dots
        d.duration_log = self.duration_log
        d.factor = self.factor
        return d

    def get_length(self, with_factor=True):
        num = (1 << (1 + self.dots)) - 1
        dot_fact = Fraction(num, 1 << self.dots) if self.dots else num

        if self.duration_log <= 0:
            base = 1 << (-self.duration_log)
        else:
            base = Fraction(1, 1 << self.duration_log)

        if with_factor:
            return base * dot_fact * self.factor
        else:
            return base * dot_fact


def set_create_midi(option):
    """
    Implement the midi command-line option '-m' and '--midi'.
    If True, add midi-block to .ly file (see L{musicexp.Score.print_ly}).

    @param option: Indicates whether the midi-block has to be added or not.
    @type option: boolean
    """
    global midi_option
    midi_option = option


def get_create_midi():
    """
    Return, if exists the state of the midi-option.

    @return: The state of the midi-option.
    @rtype: boolean
    """
    try:
        return midi_option
    except NameError:
        return False


# implement the command-line option '--transpose'
def set_transpose(option):
    global transpose_option
    transpose_option = option


def get_transpose(optType):
    try:
        if optType == "string":
            return r'\transpose c %s' % transpose_option
        elif optType == "integer":
            p = generic_tone_to_pitch(transpose_option)
            return p.semitones()
    except Exception:  # TODO: find out what the possible exception is here.
        if optType == "string":
            return ""
        elif optType == "integer":
            return 0


# implement the command-line option '--tab-clef'
def set_tab_clef(option):
    global tab_clef_option
    tab_clef_option = option


def get_tab_clef():
    try:
        return ("tab", tab_clef_option)[tab_clef_option == "tab"
                                        or tab_clef_option == "moderntab"]
    except NameError:
        return "tab"


# definitions of the command-line option '--string-numbers'
def set_string_numbers(option):
    global string_numbers_option
    string_numbers_option = option


def get_string_numbers():
    try:
        return ("t", string_numbers_option)[string_numbers_option == "t"
                                            or string_numbers_option == "f"]
    except NameError:
        return "t"


def generic_tone_to_pitch(tone):
    accidentals_dict = {
        "": 0,
        "es": -1,
        "s": -1,
        "eses": -2,
        "ses": -2,
        "is": 1,
        "isis": 2
    }
    p = Pitch()
    tone_ = tone.strip().lower()
    p.octave = tone_.count("'") - tone_.count(",")
    tone_ = tone_.replace(",", "").replace("'", "")
    p.step = ((ord(tone_[0]) - ord('a') + 5) % 7)
    p.alteration = accidentals_dict.get(tone_[1:], 0)
    return p


# Implement the different note names for the various languages
def pitch_generic(pitch, notenames, accidentals):
    s = notenames[pitch.step]
    halftones = int(pitch.alteration)
    if halftones < 0:
        s += accidentals[0] * (-halftones)
    elif pitch.alteration > 0:
        s += accidentals[3] * (halftones)
    # Handle remaining fraction to pitch.alteration (for microtones)
    if halftones != pitch.alteration:
        if None in accidentals[1:3]:
            ly.warning(
                _("Language does not support microtones "
                  "contained in the piece"))
        else:
            try:
                s += {-0.5: accidentals[1],
                      0.5: accidentals[2]}[pitch.alteration - halftones]
            except KeyError:
                ly.warning(
                    _("Language does not support microtones "
                      "contained in the piece"))
    return s


def pitch_general(pitch):
    s = pitch_generic(pitch,
                      ['c', 'd', 'e', 'f', 'g', 'a', 'b'],
                      ['es', 'eh', 'ih', 'is'])
    if "h" in s:  # no short forms for quarter tones
        return s
    return s.replace('aes', 'as').replace('ees', 'es')


def pitch_nederlands(pitch):
    return pitch_general(pitch)


def pitch_catalan(pitch):
    s = pitch_generic(pitch,
                      ['do', 're', 'mi', 'fa', 'sol', 'la', 'si'],
                      ['b', 'qb', 'qd', 'd'])
    return (s.replace('bq', 'tq')
             .replace('dq', 'tq')
             .replace('bt', 'c')
             .replace('dt', 'c'))


def pitch_deutsch(pitch):
    s = pitch_generic(pitch,
                      ['c', 'd', 'e', 'f', 'g', 'a', 'h'],
                      ['es', 'eh', 'ih', 'is'])
    if s == 'hes':
        return 'b'
    if s[0] == "a":
        return s.replace('e', 'a').replace('aa', 'a')
    return s.replace('ee', 'e')


def pitch_english(pitch):
    s = pitch_generic(pitch,
                      ['c', 'd', 'e', 'f', 'g', 'a', 'b'],
                      ['f', 'qf', 'qs', 's'])
    return s[0] + s[1:].replace('fq', 'tq').replace('sq', 'tq')


def pitch_espanol(pitch):
    s = pitch_generic(pitch,
                      ['do', 're', 'mi', 'fa', 'sol', 'la', 'si'],
                      ['b', 'cb', 'cs', 's'])
    return s.replace('bc', 'tc').replace('sc', 'tc')


def pitch_francais(pitch):
    s = pitch_generic(pitch,
                      ['do', 'ré', 'mi', 'fa', 'sol', 'la', 'si'],
                      ['b', 'sb', 'sd', 'd'])
    return s


def pitch_italiano(pitch):
    s = pitch_generic(pitch,
                      ['do', 're', 'mi', 'fa', 'sol', 'la', 'si'],
                      ['b', 'sb', 'sd', 'd'])
    return s


def pitch_norsk(pitch):
    s = pitch_generic(pitch,
                      ['c', 'd', 'e', 'f', 'g', 'a', 'h'],
                      ['ess', 'eh', 'ih', 'iss'])
    return s.replace('hess', 'b')


def pitch_portugues(pitch):
    s = pitch_generic(pitch,
                      ['do', 're', 'mi', 'fa', 'sol', 'la', 'si'],
                      ['b', 'bqt', 'sqt', 's'])
    return s.replace('bbq', 'btq').replace('ssq', 'stq')


def pitch_suomi(pitch):
    s = pitch_generic(pitch,
                      ['c', 'd', 'e', 'f', 'g', 'a', 'h'],
                      ['es', 'eh', 'ih', 'is'])
    if s == 'hes':
        return 'b'
    return s.replace('aes', 'as').replace('ees', 'es')


def pitch_svenska(pitch):
    s = pitch_generic(pitch,
                      ['c', 'd', 'e', 'f', 'g', 'a', 'h'],
                      ['ess', 'eh', 'ih', 'iss'])
    if s == 'hess':
        return 'b'
    return s.replace('aes', 'as').replace('ees', 'es')


def pitch_vlaams(pitch):
    s = pitch_generic(pitch,
                      ['do', 're', 'mi', 'fa', 'sol', 'la', 'si'],
                      ['b', 'hb', 'hk', 'k'])
    return s


def set_pitch_language(language):
    global pitch_generating_function
    function_dict = {
        "nederlands": pitch_nederlands,
        "català": pitch_catalan,
        "deutsch": pitch_deutsch,
        "english": pitch_english,
        "español": pitch_espanol,
        "français": pitch_francais,
        "italiano": pitch_italiano,
        "norsk": pitch_norsk,
        "português": pitch_portugues,
        "suomi": pitch_suomi,
        "svenska": pitch_svenska,
        "vlaams": pitch_vlaams}
    pitch_generating_function = function_dict.get(language, pitch_general)


# global variable to hold the formatting function.
pitch_generating_function = pitch_general


class Pitch:
    def __init__(self):
        self.alteration = 0
        self.step = 0
        self.octave = 0
        self._force_absolute_pitch = False

    def __repr__(self):
        return self.ly_expression()

    def transposed(self, interval):
        c = self.copy()
        c.alteration += interval.alteration
        c.step += interval.step
        c.octave += interval.octave
        c.normalize()

        target_st = self.semitones() + interval.semitones()
        c.alteration += target_st - c.semitones()
        return c

    def normalize(c):
        while c.step < 0:
            c.step += 7
            c.octave -= 1
        c.octave += c.step // 7
        c.step = c.step % 7

    def lisp_expression(self):
        return '(ly:make-pitch %d %d %d)' % (self.octave,
                                             self.step,
                                             self.alteration)

    def copy(self):
        p = Pitch()
        p.alteration = self.alteration
        p.step = self.step
        p.octave = self.octave
        p._force_absolute_pitch = self._force_absolute_pitch
        return p

    def steps(self):
        return self.step + self.octave * 7

    def semitones(self):
        return (self.octave * 12
                + [0, 2, 4, 5, 7, 9, 11][self.step]
                + self.alteration)

    def normalize_alteration(c):
        if (c.alteration < 0
                and [True, False, False, True, False, False, False][c.step]):
            c.alteration += 1
            c.step -= 1
        elif (c.alteration > 0
              and [False, False, True, False, False, False, True][c.step]):
            c.alteration -= 1
            c.step += 1
        c.normalize()

    def add_semitones(self, number):
        semi = number + self.alteration
        self.alteration = 0
        if semi == 0:
            return
        sign = (1, -1)[semi < 0]
        prev = self.semitones()
        while abs((prev + semi) - self.semitones()) > 1:
            self.step += sign
            self.normalize()
        self.alteration += (prev + semi) - self.semitones()
        self.normalize_alteration()

    def ly_step_expression(self):
        return pitch_generating_function(self)

    def absolute_pitch(self):
        if self.octave >= 0:
            return "'" * (self.octave + 1)
        elif self.octave < -1:
            return "," * (-self.octave - 1)
        else:
            return ''

    def relative_pitch(self):
        global previous_pitch
        if not previous_pitch:
            previous_pitch = self
            return self.absolute_pitch()
        previous_pitch_steps = previous_pitch.octave * 7 + previous_pitch.step
        this_pitch_steps = self.octave * 7 + self.step
        pitch_diff = (this_pitch_steps - previous_pitch_steps)
        previous_pitch = self
        if pitch_diff > 3:
            return "'" * ((pitch_diff + 3) // 7)
        elif pitch_diff < -3:
            return "," * ((-pitch_diff + 3) // 7)
        else:
            return ""

    def ly_expression(self):
        s = self.ly_step_expression()
        if relative_pitches and not self._force_absolute_pitch:
            s += self.relative_pitch()
        else:
            s += self.absolute_pitch()
        return s

    def print_ly(self, outputter, pitch_mods=''):
        outputter('%s%s' % (self.ly_expression(), pitch_mods))


class Base:
    def contains(self, elem):
        return self == elem


class Music(Base):
    def __init__(self):
        self.parent = None
        self.start = 0
        self.comment = ''
        self.identifier = None
        self.color = None

    def get_length(self, with_factor=True):
        return 0

    def get_properties(self):
        return ''

    def get_index(self):
        if self.parent:
            return self.parent.elements.index(self)
        else:
            return None

    def name(self):
        return self.__class__.__name__

    def lisp_expression(self):
        name = self.name()

        props = self.get_properties()

        return "(make-music '%s %s)" % (name, props)

    def set_start(self, start):
        self.start = start

    def find_first(self, predicate):
        if predicate(self):
            return self
        return None

    def print_comment(self, printer, text=None):
        if not text:
            text = self.comment

        if not text:
            return

        if text == '\n':
            printer.newline()
            return

        lines = text.split('\n')
        for l in lines:
            if l:
                printer.unformatted_output('% ' + l)
            printer.newline()

    def print_with_identifier(self, printer):
        if self.identifier:
            printer(r"\%s" % self.identifier)
        else:
            self.print_ly(printer)

    def print_ly(self, printer):
        printer(self.ly_expression())


class MusicWrapper(Music):
    def __init__(self):
        Music.__init__(self)
        self.element = None

    def contains(self, elem):
        return self == elem or self.element.contains(elem)

    def print_ly(self, func):
        self.element.print_ly(func)


class ModeChangingMusicWrapper(MusicWrapper):
    def __init__(self):
        MusicWrapper.__init__(self)
        self.mode = 'notemode'

    def print_ly(self, func):
        func(r'\%s' % self.mode)
        MusicWrapper.print_ly(self, func)


class RelativeMusic(MusicWrapper):
    def __init__(self):
        MusicWrapper.__init__(self)
        self.basepitch = None

    def print_ly(self, func):
        global previous_pitch
        global relative_pitches
        prev_relative_pitches = relative_pitches
        relative_pitches = True
        previous_pitch = self.basepitch
        if not previous_pitch:
            previous_pitch = Pitch()
        func(r'\relative %s%s' % (pitch_generating_function(previous_pitch),
                                  previous_pitch.absolute_pitch()))
        MusicWrapper.print_ly(self, func)
        relative_pitches = prev_relative_pitches


class TimeScaledMusic(MusicWrapper):
    def __init__(self):
        MusicWrapper.__init__(self)
        self.numerator = 1
        self.denominator = 1
        self.display_number = "actual"  # valid: "actual" | "both" | None
        # Display basic note length for the tuplet if set.
        self.display_type = None  # valid: "actual" | "both" | None
        self.display_bracket = "bracket"  # valid: "bracket" | "curved" | None
        self.actual_type = None   # The actually played unit of the scaling.
        self.normal_type = None   # The basic unit of the scaling.
        self.display_numerator = None
        self.display_denominator = None
        self.force_direction = 0
        self.visible = True

    def print_ly(self, func):
        if self.display_bracket is None:
            func(r"\tweak TupletBracket.stencil ##f")
        elif self.display_bracket == "curved":
            func(r"\tweak TupletBracket.tuplet-slur ##t")

        dir = {
            -1: r'\tweak TupletBracket.direction #DOWN',
            1: r'\tweak TupletBracket.direction #UP'
        }.get(self.force_direction, '')
        if dir:
            func(dir)

        base_number_function = {
            None: "#f",
            "actual": "tuplet-number::calc-denominator-text",
            "both": "tuplet-number::calc-fraction-text"
        }.get(self.display_number, None)
        # If we have non-standard numerator/denominator, use our custom
        # function
        if self.display_number == "actual" and self.display_denominator:
            base_number_function = (
                "(tuplet-number::non-default-tuplet-denominator-text %s)"
                % self.display_denominator)
        elif (self.display_number == "both"
              and (self.display_denominator or self.display_numerator)):
            if self.display_numerator:
                num = self.display_numerator
            else:
                num = "#f"
            if self.display_denominator:
                den = self.display_denominator
            else:
                den = "#f"
            base_number_function = (
                "(tuplet-number::non-default-tuplet-fraction-text %s %s)"
                % (den, num))

        if self.display_type == "actual" and self.normal_type:
            base_duration = self.normal_type.lisp_expression()
            func(r"\tweak TupletNumber.text")
            func("#(tuplet-number::append-note-wrapper %s %s)"
                 % (base_number_function, base_duration))
        # TODO: Implement this using actual_type and normal_type!
        elif self.display_type == "both":
            if self.display_number is None:
                func(r"\tweak TupletNumber.stencil ##f")
            elif self.display_number == "both":
                den_duration = self.normal_type.lisp_expression()
                # If we don't have an actual type set, use the normal duration!
                if self.actual_type:
                    num_duration = self.actual_type.lisp_expression()
                else:
                    num_duration = den_duration
                if self.display_denominator or self.display_numerator:
                    func(r"\tweak TupletNumber.text")
                    func("#(tuplet-number::non-default-fraction-with-notes "
                         "%s %s %s %s)"
                         % (self.display_denominator,
                            den_duration,
                            self.display_numerator,
                            num_duration))
                else:
                    func(r"\tweak TupletNumber.text")
                    func("#(tuplet-number::fraction-with-notes %s %s)"
                         % (den_duration, num_duration))
        else:
            if self.display_number is None:
                func(r"\tweak TupletNumber.stencil ##f")
            elif self.display_number == "both":
                func(r"\tweak TupletNumber.text #%s" %
                     base_number_function)

        if not self.visible:
            func(r'\tweak TupletBracket.transparent ##t')
            func(r'\tweak TupletNumber.transparent ##t')
        func(r'\tuplet')
        func.print_verbatim(' %d/%d' % (self.denominator, self.numerator))
        func.add_factor(Fraction(self.numerator, self.denominator))
        MusicWrapper.print_ly(self, func)
        func.revert()


class NestedMusic(Music):
    def __init__(self):
        Music.__init__(self)
        self.elements = []

    def append(self, what):
        if what:
            self.elements.append(what)

    def contains(self, elem):
        if self == elem:
            return True
        for e in self.elements:
            if e.contains(elem):
                return True
        return False

    def insert_around(self, succ, elt, dir):
        assert elt.parent is None
        assert succ is None or succ in self.elements

        idx = 0
        if succ:
            idx = self.elements.index(succ)
            if dir > 0:
                idx += 1
        else:
            if dir < 0:
                idx = 0
            elif dir > 0:
                idx = len(self.elements)

        self.elements.insert(idx, elt)
        elt.parent = self

    def get_properties(self):
        return ("'elements (list %s)"
                % " ".join([x.lisp_expression() for x in self.elements]))

    def get_subset_properties(self, predicate):
        return ("'elements (list %s)"
                % " ".join([x.lisp_expression() for x
                            in list(filter(predicate, self.elements))]))

    def get_neighbor(self, music, dir):
        assert music.parent == self
        idx = self.elements.index(music)
        idx += dir
        idx = min(idx, len(self.elements) - 1)
        idx = max(idx, 0)

        return self.elements[idx]

    def delete_element(self, element):
        assert element in self.elements

        self.elements.remove(element)
        element.parent = None

    def set_start(self, start):
        self.start = start
        for e in self.elements:
            e.set_start(start)

    def find_first(self, predicate):
        r = Music.find_first(self, predicate)
        if r:
            return r

        for e in self.elements:
            r = e.find_first(predicate)
            if r:
                return r
        return None


class SequentialMusic(NestedMusic):
    def get_last_event_chord(self):
        value = None
        at = len(self.elements) - 1
        while (at >= 0
               and not isinstance(self.elements[at], ChordEvent)
               and not isinstance(self.elements[at], BarLine)):
            at -= 1

        if at >= 0 and isinstance(self.elements[at], ChordEvent):
            value = self.elements[at]
        return value

    def print_ly(self, printer, newline=True):
        printer('{')
        if self.comment:
            self.print_comment(printer)

        if newline:
            printer.newline()
        for e in self.elements:
            e.print_ly(printer)

        printer('}')
        if newline:
            printer.newline()

    def lisp_sub_expression(self, pred):
        name = self.name()

        props = self.get_subset_properties(pred)

        return "(make-music '%s %s)" % (name, props)

    def set_start(self, start):
        for e in self.elements:
            e.set_start(start)
            start += e.get_length()


class RepeatedMusic(Base):
    def __init__(self):
        self.repeat_type = "volta"
        self.repeat_count = 2
        self.endings = []
        self.tremolo_strokes = None
        self.music = None

    def set_music(self, music):
        if isinstance(music, Music):
            self.music = music
        elif isinstance(music, list):
            self.music = SequentialMusic()
            self.music.elements = music
        else:
            ly.warning(_("unable to set the music %(music)s "
                         "for the repeat %(repeat)s")
                       % {'music': music, 'repeat': self})

    def add_ending(self, music):
        self.endings.append(music)

    def contains(self, elem):
        return self == elem or self.music.contains(elem)

    def print_ly(self, printer):
        if self.tremolo_strokes is not None:
            # We can't use `\tweak` here.
            printer.dump(r'\once \override Beam.gap-count = %s'
                         % self.tremolo_strokes)
        printer.dump(r'\repeat %s %s' % (self.repeat_type, self.repeat_count))
        if self.music:
            # No extra newlines for a tremolo group.
            self.music.print_ly(printer, self.repeat_type != 'tremolo')
        else:
            ly.warning(_("encountered repeat without body"))
            printer.dump('{}')
        if self.endings:
            printer.dump(r'\alternative {')
            for e in self.endings:
                e.print_ly(printer)
            printer.dump('}')


class Lyrics(Base):
    def __init__(self):
        self.lyrics_syllables = []
        self.stanza_id = None

    def lyrics_to_ly(self):
        mode = r'\lyricmode {'
        no_melismata = r'\set ignoreMelismata = ##t'
        lyrics = ''
        for l in self.lyrics_syllables:
            lyrics += l
        return (mode, no_melismata, lyrics)

    def print_ly(self, printer):
        (mode, no_melismata, lyrics) = self.lyrics_to_ly()
        printer.dump(mode)
        printer.newline()

        printer.dump(no_melismata)
        printer.newline()

        printer.dump_lyrics(lyrics)
        printer.newline()

        printer.dump('}')
        printer.newline()

    def ly_expression(self):
        return "%s %s %s" % self.lyrics_to_ly()


class Header(Base):
    def __init__(self):
        self.header_fields = {}

    def set_field(self, field, value):
        self.header_fields[field] = value

    def format_header_strings(self, key, value, printer):
        printer.dump(key + ' =')

        # If a header item contains a line break, it is segmented. The
        # substrings are formatted with the help of \markup, using
        # \column and \line. An exception, however, are texidoc items,
        # which should not contain LilyPond formatting commands.
        if key == 'texidoc':
            printer.dump_texidoc(value)
        else:
            if '\n' in value:
                value = value.replace('"', '')
                printer.dump(r'\markup \column {')
                substrings = value.split('\n')
                for s in substrings:
                    printer.newline()
                    printer.dump(r'\line { "' + s + '" }')
                printer.newline()
                printer.dump('}')
                printer.newline()
            else:
                printer.dump(value)
        printer.newline()

    def print_ly(self, printer):
        printer.dump(r"\header {")
        printer.newline()
        for (k, v) in list(self.header_fields.items()):
            if v:
                self.format_header_strings(k, v, printer)
        # printer.newline()
        printer.dump("}")
        printer.newline()
        printer.newline()


class Paper(Base):
    def __init__(self):
        self.global_staff_size = -1
        # page size
        self.page_width = -1
        self.page_height = -1
        # page margins
        self.top_margin = -1
        self.bottom_margin = -1
        self.left_margin = -1
        self.right_margin = -1
        self.system_left_margin = -1
        self.system_right_margin = -1
        self.system_distance = -1
        self.top_system_distance = -1
        self.indent = 0
        self.short_indent = 0
        self.instrument_names = []

    def print_length_field(self, printer, field, value):
        if value >= 0:
            printer.dump(r"%s = %s\cm" % (field, value))
            printer.newline()

    def get_longest_instrument_name(self):
        result = ''
        for name in self.instrument_names:
            lines = name.split('\n')
            for line in lines:
                if len(line) > len(result):
                    result = line
        return result

    def print_ly(self, printer):
        if self.global_staff_size > 0:
            printer.dump('#(set-global-staff-size %s)' %
                         self.global_staff_size)
            printer.newline()
        printer.dump(r'\paper {')
        printer.newline()
        self.print_length_field(printer, "paper-width", self.page_width)
        self.print_length_field(printer, "paper-height", self.page_height)
        self.print_length_field(printer, "top-margin", self.top_margin)
        self.print_length_field(printer, "bottom-margin", self.bottom_margin)
        self.print_length_field(printer, "left-margin", self.left_margin)
        # TODO: maybe set line-width instead of right-margin?
        self.print_length_field(printer, "right-margin", self.right_margin)
        # TODO: What's the corresponding setting for system_left_margin and
        #        system_right_margin in LilyPond?
        self.print_length_field(
            printer, "between-system-space", self.system_distance)
        self.print_length_field(
            printer, "page-top-space", self.top_system_distance)
        # TODO: Compute the indentation with the instrument name lengths

        # TODO: font width ?
        char_per_cm = (len(self.get_longest_instrument_name())
                       * 13) / self.page_width
        if self.indent != 0:
            self.print_length_field(
                printer, "indent", self.indent / char_per_cm)
        if self.short_indent != 0:
            self.print_length_field(
                printer, "short-indent", self.short_indent / char_per_cm)

        printer.dump('}')
        printer.newline()
        printer.newline()


class Layout(Base):
    def __init__(self):
        self.context_dict = {}

    def add_context(self, context):
        if context not in self.context_dict:
            self.context_dict[context] = []

    def set_context_item(self, context, item):
        self.add_context(context)
        if item not in self.context_dict[context]:
            self.context_dict[context].append(item)

    def print_ly(self, printer):
        if list(self.context_dict.items()):
            printer.dump(r'\layout {')
            printer.newline()
            for (context, defs) in list(self.context_dict.items()):
                printer.dump(r'\context {')
                printer.newline()
                printer.dump(r'\%s' % context)
                printer.newline()
                for d in defs:
                    printer.dump(d)
                    printer.newline()
                printer.dump('}')
                printer.newline()
            printer.dump('}')
            printer.newline()
            printer.newline()


class ChordEvent(NestedMusic):
    def __init__(self):
        NestedMusic.__init__(self)
        self.after_grace_elements = None
        self.grace_elements = None
        self.grace_type = None

    def append_grace(self, element):
        if element:
            if not self.grace_elements:
                self.grace_elements = SequentialMusic()
            self.grace_elements.append(element)

    def append_after_grace(self, element):
        if element:
            if not self.after_grace_elements:
                self.after_grace_elements = SequentialMusic()
            self.after_grace_elements.append(element)

    def has_elements(self):
        return [e for e in self.elements
                if isinstance(e, NoteEvent) or isinstance(e, RestEvent)] != []

    def get_length(self, with_factor=True):
        if self.elements:
            return max([e.get_length(with_factor) for e in self.elements])
        else:
            return 0

    def get_duration(self):
        note_events = [e for e in self.elements
                       if isinstance(e, NoteEvent) or isinstance(e, RestEvent)]
        if note_events:
            return note_events[0].duration
        else:
            return None

    def print_ly(self, printer):
        staff_changes = [e for e in self.elements
                         if isinstance(e, StaffChange)]

        note_events = [e for e in self.elements
                       if isinstance(e, NoteEvent)]

        rest_events = [e for e in self.elements
                       if isinstance(e, RhythmicEvent)
                       and not isinstance(e, NoteEvent)]

        other_events = [e for e in self.elements
                        if not (isinstance(e, RhythmicEvent)
                                or isinstance(e, StaffChange))]

        if self.after_grace_elements:
            printer(r'\afterGrace {')

        if self.grace_elements and self.elements:
            # TODO: Support slashed grace beams.
            if self.grace_type:
                printer(r'\slashedGrace')
            else:
                printer(r'\grace')
            # Don't print a newline after a braced grace group.
            self.grace_elements.print_ly(printer, False)
        elif self.grace_elements:  # No `self.elements`!
            ly.warning(_("Grace note with no following music: %s") %
                       self.grace_elements)
            if self.grace_type:
                printer(r'\%s' % self.grace_type)
            else:
                printer(r'\grace')
            self.grace_elements.print_ly(printer, False)
            printer('{}')

        if staff_changes:
            staff_changes[0].print_ly(printer)

        # Print all overrides and other settings needed by the
        # articulations/ornaments before the note

        for e in other_events:
            f = getattr(e, 'print_before_note', None)
            if f is not None:
                f(printer)

        if rest_events:
            rest_events[0].print_ly(printer)
        elif len(note_events) == 1:
            note_events[0].print_ly(printer)
        elif note_events:
            global previous_pitch
            pitches = []
            basepitch = None
            stem = None
            for x in note_events:
                if x.associated_events:
                    for aev in x.associated_events:
                        if isinstance(aev, StemEvent) and aev.value:
                            stem = aev
                pitches.append(x.chord_element_ly())
                if not basepitch:
                    basepitch = previous_pitch
            if stem:
                printer(stem.ly_expression())
            printer('<%s>' % ' '.join(pitches))
            previous_pitch = basepitch
            duration = self.get_duration()
            if duration:
                duration.print_ly(printer)
        else:
            pass

        for e in other_events:
            e.print_ly(printer)

        for e in other_events:
            f = getattr(e, 'print_after_note', None)
            if f is not None:
                f(printer)

        if self.after_grace_elements:
            printer('}')
            self.after_grace_elements.print_ly(printer, False)

        self.print_comment(printer)


class Partial(Music):
    def __init__(self):
        Music.__init__(self)
        self.partial = None

    def print_ly(self, printer):
        if self.partial:
            printer.dump(r"\partial %s" % self.partial.ly_expression())


class BarLine(Music):
    def __init__(self):
        Music.__init__(self)
        self.bar_number = 0  # The bar number of the bar to the right.
        self.type = None

    def print_ly(self, printer):
        bar_symbol = {
            'dashed': '!',
            'dotted': ';',
            'heavy': '.',
            'heavy-heavy': '..',
            'heavy-light': '.|',
            'light-heavy': '|.',
            'light-light': '||',
            'none': '',
            'regular': '|',
            'short': ',',
            'tick': "'"}.get(self.type, None)
        if bar_symbol is not None:
            printer.dump(r'\bar "%s"' % bar_symbol)
        else:
            printer.dump("|")

        # Emit a comment indicating the bar number to the left.
        if self.bar_number > 1:
            printer.print_verbatim(' %% %d' % (self.bar_number - 1))
            if self.bar_number % 10 == 0:
                printer.newline()
                printer.newline()
                printer.dump(r"\barNumberCheck #%d " % self.bar_number)
        printer.newline()

    def ly_expression(self):
        return " | "


class Event(Music):
    def __init__(self):
        # strings to print before the note to which an event is attached.
        # Ignored for notes etc.
        super(Event, self).__init__()
        self.before_note = None
        self.after_note = None

    # print something before the note to which an event is attached,
    # e.g., overrides
    def print_before_note(self, printer):
        if self.before_note:
            printer.dump(self.before_note)

    # print something after the note to which an event is attached,
    # e.g., resetting
    def print_after_note(self, printer):
        if self.after_note:
            printer.dump(self.after_note)
    pass


class SpanEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.mxl_event = None  # For accessing the other part of the spanner.
        self.mxl_attributes = None
        self.span_direction = 0  # start/stop
        self.line_type = 'solid'
        self.span_type = 0  # e.g. cres/decrescendo, ottava up/down
        self.size = 0  # size of e.g. octave shift
        self.force_direction = 0  # for LilyPond's `^` and `_` modifier
        self.visible = True

    def wait_for_note(self):
        return True

    def get_properties(self):
        return "'span-direction %d" % self.span_direction

    def set_span_type(self, type):
        self.span_type = type

    def not_visible(self):
        if self.visible:
            return ''
        else:
            return r'\tweak transparent ##t '

    def get_mxl_event_attribute(self, attribute, default):
        ret = default
        if self.mxl_attributes is not None:
            ret = self.mxl_attributes.get(attribute, default)
        elif self.mxl_event is not None:
            ret = getattr(self.mxl_event, attribute, default)
        return ret

    def get_paired_mxl_event_attribute(self, attribute, default):
        ret = default
        if (self.mxl_event is not None
                and self.mxl_event.paired_with is not None):
            paired = self.mxl_event.paired_with.spanner_event
            if paired is not None:
                if paired.mxl_attributes is not None:
                    ret = paired.mxl_attributes.get(attribute, default)
                elif paired.mxl_event is not None:
                    ret = getattr(paired.mxl_event, attribute, default)
        return ret

    def get_paired_event(self):
        ret = None
        if (self.mxl_event is not None
                and self.mxl_event.paired_with is not None):
            ret = self.mxl_event.paired_with.spanner_event
        return ret

    def get_paired_text_elements(self):
        ret = None
        if (self.mxl_event is not None
                and self.mxl_event.paired_with is not None):
            paired_event = self.mxl_event.paired_with.spanner_event
            if paired_event is not None:
                ret = paired_event.text_elements
        return ret


class BreatheEvent(Event):
    def __init__(self):
        super().__init__()
        self.after_note = r"\breathe"

    def ly_expression(self):
        return ''


class CaesuraEvent(Event):
    def __init__(self):
        super().__init__()
        self.after_note = r"\caesura"

    def ly_expression(self):
        return ''


class SlurEvent(SpanEvent):
    def print_before_note(self, printer):
        command = {'dotted': r'\slurDotted',
                   'dashed': r'\slurDashed'}.get(self.line_type, '')
        if command and self.span_direction == -1:
            printer.dump(command)

    def print_after_note(self, printer):
        # reset non-solid slur types!
        command = {'dotted': r'\slurSolid',
                   'dashed': r'\slurSolid'}.get(self.line_type, '')
        if command and self.span_direction == -1:
            printer.dump(command)

    def direction_mod(self):
        return {1: '^', -1: '_', 0: ''}.get(self.force_direction, '')

    def slur_to_ly(self):
        return {-1: '(', 1: ')'}.get(self.span_direction, '')

    def ly_expression(self):
        return self.slur_to_ly()

    def print_ly(self, printer):
        val = self.slur_to_ly()
        if val:
            if self.span_direction == -1:
                printer.dump('%s%s%s' % (super().not_visible(),
                                         self.direction_mod(), val))
            else:
                printer.dump(val)


class BeamEvent(SpanEvent):
    def ly_expression(self):
        return {-1: '[', 1: ']'}.get(self.span_direction, '')


class PedalEvent(SpanEvent):
    # LilyPond's support for positioning pedal marks above or below a staff
    # is limited: if there is a series of `\sustainOn` and `\sustainOff`
    # commands without any intermediate stop, all of them are positioned
    # with a single `SustainPedalLineSpanner` grob (which can span over
    # multiple systems).  In other words, positioning of single pedal marks
    # is not possible in general.  For this reason we ignore the `placement`
    # attribute.
    def ly_expression(self):
        return {-1: r'\sustainOn',
                0: r'\sustainOff\sustainOn',
                1: r'\sustainOff'}.get(self.span_direction, '')


class TextSpannerEvent(SpanEvent):
    def __init__(self):
        SpanEvent.__init__(self)
        self.text_elements = None
        self.start_stop = False  # for single-note spanners

    def direction_mod(self):
        return {1: '^', -1: '_', 0: ''}.get(self.force_direction, '')

    def text_spanner_to_ly(self):
        global whatOrnament
        style = getattr(self, 'style', None)

        if style == 'ignore':
            return ([], '')

        if whatOrnament == "wave":
            return {-1: ([r"\tweak style #'trill"], r'\startTextSpan'),
                    1: ([], r'\stopTextSpan')}.get(self.span_direction,
                                                   ([], ''))

        elif style == "dashes":
            val = ''
            tweaks = []

            if self.span_direction == -1:
                val = r'\startTextSpan'
                tweaks.append(r"\tweak style #'dashed-line")

                start_markup = text_to_ly(self.text_elements,
                                          r'\normal-text')
                if start_markup:
                    tweaks.append(r'\tweak bound-details.left.text '
                                  + r'\markup ' + start_markup)

                stop_markup = text_to_ly(self.get_paired_text_elements(),
                                         r'\normal-text')
                if stop_markup:
                    tweaks.append(r'\tweak bound-details.right.text '
                                  + r'\markup ' + stop_markup)
            elif self.span_direction == 1:
                if isinstance(self.get_paired_event(), DynamicsSpannerEvent):
                    val = r'\!'
                else:
                    val = r'\stopTextSpan'

            return (tweaks, val)

        elif style == 'stop' and whatOrnament != 'trill':
            return ([], '')

        return {-1: ([], r'\startTrillSpan'),
                1: ([], r'\stopTrillSpan')}.get(self.span_direction,
                                                ([], ''))

    def ly_expression(self):
        (tweaks, val) = self.text_spanner_to_ly()

        ret = ''
        if val:
            not_visible = super().not_visible()
            if tweaks:
                ret = '%s%s %s' % (not_visible, ' '.join(tweaks), val)
            else:
                ret = '%s%s' % (not_visible, val)
        return ret

    def print_ly(self, printer):
        (tweaks, val) = self.text_spanner_to_ly()

        if val:
            if self.span_direction == -1:
                not_visible = super().not_visible()
                if tweaks:
                    printer('%s%s' % (not_visible, tweaks[0]))
                    for tweak in tweaks[1:]:
                        printer(tweak)
                    printer('%s%s' % (self.direction_mod(), val))
                else:
                    printer('%s%s%s' % (not_visible,
                                        self.direction_mod(), val))
            else:
                printer(val)


# This class gets used only for the start part of a spanner; the
# corresponding end part is always of type `TextSpannerEvent`.
class DynamicsSpannerEvent(SpanEvent):
    def __init__(self):
        SpanEvent.__init__(self)
        self.text_elements = None
        self.type = None

    def wait_for_note(self):
        return False

    def direction_mod(self):
        return {1: '^', -1: '_', 0: ''}.get(self.force_direction, '')

    def dynamics_spanner_to_ly(self):
        text_markup = text_to_ly(self.text_elements, r'\normal-text')

        # We can't use `\tweak` here.
        overrides = []
        if self.type == 'cresc':
            overrides.append(r'\once \set crescendoText = \markup '
                             + text_markup)
            overrides.append(r"\once \set crescendoSpanner = #'text")
        else:
            overrides.append(r'\once \set decrescendoText = \markup '
                             + text_markup)
            overrides.append(r"\once \set decrescendoSpanner = #'text")

        if not self.visible:
            overrides.append(r'\once \override '
                             r'DynamicTextSpanner.transparent = ##t')

        val = {'cresc': r'\<',
               'dim': r'\>'}.get(self.type, '')

        return (overrides, val)

    def ly_expression(self):
        (overrides, val) = self.dynamics_spanner_to_ly()

        return '%s <>%s' % (' '.join(overrides), val)

    def print_ly(self, printer):
        (overrides, val) = self.dynamics_spanner_to_ly()

        for override in overrides:
            printer('%s' % override)

        printer('<>%s%s' % (self.direction_mod(), val))


class BracketSpannerEvent(SpanEvent):
    # Ligature brackets use prefix notation for the start.
    def wait_for_note(self):
        if self.span_direction == -1:
            return False
        else:
            return True

    def bracket_to_ly(self):
        return {1: r'\]', -1: r'\['}.get(self.span_direction, '')

    def ly_expression(self):
        return self.bracket_to_ly()

    def print_ly(self, printer):
        val = self.bracket_to_ly()
        if val:
            if self.span_direction == -1:
                style = {"dashed": "dashed-line",
                         "dotted": "dotted-line",
                         "wavy": "trill"}.get(self.line_type, None)
                if style:
                    printer.dump(r"\tweak style #'%s" % style)

                line_end_at_start = \
                    self.get_mxl_event_attribute('line-end', 'none')
                line_end_at_stop = \
                    self.get_paired_mxl_event_attribute('line-end', 'none')
                printer.dump(r"\tweak edge-height #(make-edge-height '%s '%s)"
                             % (line_end_at_start, line_end_at_stop))

                dir = {1: "#UP",
                       -1: "#DOWN"}.get(self.force_direction, '')
                if dir:
                    override = r"\tweak direction "
                    printer.dump('%s%s' % (override, dir))

            printer.dump(val)


class OctaveShiftEvent(SpanEvent):
    def wait_for_note(self):
        return False

    def set_span_type(self, type):
        self.span_type = {'up': 1, 'down': -1}.get(type, 0)

    def ly_octave_shift_indicator(self):
        # convert 8/15 to lilypond indicators (+-1/+-2)
        try:
            value = {8: 1, 15: 2}[self.size]
        except KeyError:
            ly.warning(
                _("Invalid octave shift size found: %s. Using no shift.") %
                self.size)
            value = 0
        # negative values go up!
        value *= -1 * self.span_type
        return value

    def ly_expression(self):
        dir = self.ly_octave_shift_indicator()
        value = ''
        if dir:
            value = r'\ottava #%s' % dir
        return {
            - 1: value,
            1: r'\ottava #0'}.get(self.span_direction, '')


class GlissandoEvent(SpanEvent):
    def print_before_note(self, printer):
        if self.span_direction == -1:
            style = {"dashed": "dashed-line",
                     "dotted": "dotted-line",
                     "wavy": "trill"}. get(self.line_type, None)
            if style:
                printer.dump(
                    r"\once \override Glissando.style = #'%s" % style)

    def ly_expression(self):
        return {-1: r'\glissando',
                1: ''}.get(self.span_direction, '')

    def print_ly(self, printer):
        val = self.ly_expression()
        if val:
            if self.span_direction == -1:
                printer.dump('%s%s' % (super().not_visible(), val))
            else:
                printer.dump(val)


class ArpeggioEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.direction = 0
        self.non_arpeggiate = False

    def wait_for_note(self):
        return True

    def print_before_note(self, printer):
        if self.non_arpeggiate:
            printer.dump(r"\arpeggioBracket")
        else:
            dir = {-1: r"\arpeggioArrowDown",
                   1: r"\arpeggioArrowUp"}.get(self.direction, '')
            if dir:
                printer.dump(dir)

    def print_after_note(self, printer):
        if self.non_arpeggiate or self.direction:
            printer.dump(r"\arpeggioNormal")

    def ly_expression(self):
        return r'\arpeggio'


class TieEvent(Event):
    def ly_expression(self):
        return '~'


class HairpinEvent(SpanEvent):
    def set_span_type(self, type):
        self.span_type = {'crescendo': 1,
                          'decrescendo': -1,
                          'diminuendo': -1}.get(type, 0)

    def hairpin_to_ly(self):
        if self.span_direction == 1:
            return r'\!'
        else:
            return {1: r'\<', -1: r'\>'}.get(self.span_type, '')

    def direction_mod(self):
        return {1: '^', -1: '_', 0: '-'}.get(self.force_direction, '-')

    def ly_expression(self):
        return self.hairpin_to_ly()

    def print_ly(self, printer):
        val = self.hairpin_to_ly()
        if val:
            if self.span_direction == -1:
                printer.dump('%s%s' % (self.direction_mod(), val))
            else:
                printer.dump(val)


class DynamicsEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.type = None
        self.force_direction = 0

    def wait_for_note(self):
        return True

    def ly_expression(self):
        if self.type:
            return r'\%s' % self.type
        else:
            return

    def direction_mod(self):
        return {1: '^', -1: '_', 0: '-'}.get(self.force_direction, '-')

    def print_ly(self, printer):
        if self.type:
            printer.dump(r'%s\%s' % (self.direction_mod(), self.type))


class MarkEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.text_elements = None
        self.force_direction = None

    def wait_for_note(self):
        return False

    def ly_expression(self):
        return r'\mark \markup %s' % text_to_ly(self.text_elements)

    def print_ly(self, print):
        dir = {1: '#UP',
               -1: '#DOWN'}.get(self.force_direction, '')
        if dir:
            print(r'\tweak direction %s' % dir)
        print(self.ly_expression())


class TextMarkEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.text_elements = None
        self.force_direction = None

    def wait_for_note(self):
        return False

    def ly_expression(self):
        return r'\textMark \markup %s' % text_to_ly(self.text_elements)

    def print_ly(self, print):
        dir = {1: '#UP',
               -1: '#DOWN'}.get(self.force_direction, '')
        if dir:
            print(r'\tweak direction %s' % dir)
        print(self.ly_expression())


def text_to_ly(elements, init_markup=None):
    font_weight_dict = {
        'normal': '',
        'bold': r'\bold',
    }
    font_style_dict = {
        'normal': '',
        'italic': r'\italic',
    }
    underline_dict = {
        0: '',
        1: r'\underline',
        2: r'\underline \underline',
        3: r'\underline \underline \underline',
    }
    # TODO: Support more `enclosure` values.
    enclosure_dict = {
        'none': '',
        'square': r'\square',
        'rectangle': r'\box',
        'circle': r'\circle',
        'oval': r'\ellipse',
    }

    # TODO: Handle `font-family` and other missing attributes.

    if not elements:
        return ''

    markup = []

    # The MusicXML standard doesn't specify whether a group of elements with
    # `enclosure="foo"` should be enclosed by a single 'foo', or whether
    # each element should get a separate enclosure by 'foo'.  Tests with
    # Finale and MuseScore show that they do the former, and we follow.
    enclosure_attribute = elements[0][1].get('enclosure', 'none')
    enclosure = enclosure_dict.get(enclosure_attribute, '')
    if enclosure:
        markup.append(enclosure)
    prev_enclosure = enclosure

    if init_markup is not None:
        markup.append(init_markup)

    concat = (elements[0][0].get_name() == 'lilypond-markup'
              or len(elements) > 1)
    if concat:
        markup.append(r'\concat {')

    for (element, attributes) in elements:
        enclosure_attribute = attributes.get('enclosure', 'none')
        enclosure = enclosure_dict.get(enclosure_attribute, '')
        if prev_enclosure != enclosure:
            # At this point we certainly have more than one element.
            markup.append('}')
            if enclosure:
                markup.append(enclosure)
            markup.append(r'\concat {')

            prev_enclosure = enclosure

        font_weight_attribute = attributes.get('font-weight', 'normal')
        font_weight = font_weight_dict.get(font_weight_attribute, '')
        if font_weight:
            markup.append(font_weight)

        font_style_attribute = attributes.get('font-style', 'normal')
        font_style = font_style_dict.get(font_style_attribute, '')
        if font_style:
            markup.append(font_style)

        underline_attribute = int(attributes.get('underline', 0))
        underline = underline_dict.get(underline_attribute, '')
        if underline:
            markup.append(underline)

        text = ''
        name = element.get_name()
        if name == 'words' or name == 'rehearsal':
            text = element.get_text()
            if attributes.get('xml:space', 'default') != 'preserve':
                # We use Python's special algorithm of `split()`, which
                # kicks in if there is no separator argument, to eliminate
                # runs of consecutive whitespace characters.  We also want
                # this for leading and trailing whitespace, i.e., they
                # should be treated similarly to in-between whitespace
                # (instead of being removed completely).
                text = '|' + text + '|'
                text = ' '.join(text.split())
                text = text[1:-1]
            text = utilities.escape_ly_output_string(text)
        elif name == 'segno':
            text = r'\fontsize #2 \segno'
        elif name == 'coda':
            text = r'\fontsize #2 \coda'
        elif name == 'lilypond-markup':
            text = element.get_text()
        else:
            pass  # XXX

        markup.append(text)

    if concat:
        markup.append('}')

    return ' '.join(markup)


class TextEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.text_elements = None
        self.force_direction = None

    def wait_for_note(self):
        r""" This is problematic: LilyPond markup like `^"text"` requires
        `wait_for_note` to be true, otherwise compilation will fail; we are
        thus forced to return `True`.  However, this might lead to wrong
        placement of text if derived from `<direction-type>` combinations
        not handled specially in `musicxml_direction_to_lily`.
        """
        return True

    def direction_mod(self):
        return {1: '^', -1: '_', 0: '-'}.get(self.force_direction, '-')

    def ly_expression(self):
        return r'%s\markup %s' % (self.direction_mod(),
                                  text_to_ly(self.text_elements))


class ArticulationEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.type = None
        self.force_direction = None

    def wait_for_note(self):
        return True

    def direction_mod(self):
        return {1: '^', -1: '_', 0: '-'}.get(self.force_direction, '')

    def ly_expression(self):
        return r'%s\%s' % (self.direction_mod(), self.type)


class ShortArticulationEvent(ArticulationEvent):
    def direction_mod(self):
        # default is -
        return {1: '^', -1: '_', 0: '-'}.get(self.force_direction, '-')

    def ly_expression(self):
        if self.type:
            return '%s%s' % (self.direction_mod(), self.type)
        else:
            return ''


class NoDirectionArticulationEvent(ArticulationEvent):
    def ly_expression(self):
        if self.type:
            return r'\%s' % self.type
        else:
            return ''


class MarkupEvent(ShortArticulationEvent):
    def __init__(self):
        ArticulationEvent.__init__(self)
        self.contents = None

    def ly_expression(self):
        if self.contents:
            return r"%s\markup { %s }" % (self.direction_mod(), self.contents)
        else:
            return ''


class FretEvent(MarkupEvent):
    def __init__(self):
        MarkupEvent.__init__(self)
        self.force_direction = 1
        self.strings = 6
        self.frets = 4
        self.barre = None
        self.elements = []

    def ly_expression(self):
        val = ""
        if self.strings != 6:
            val += "w:%s;" % self.strings
        if self.frets != 4:
            val += "h:%s;" % self.frets
        if self.barre and len(self.barre) >= 3:
            val += "c:%s-%s-%s;" % (self.barre[0], self.barre[1],
                                    self.barre[2] + get_transpose("integer"))
        have_fingering = False
        for i in self.elements:
            if len(i) > 1:
                val += "%s-%s" % (i[0], i[1] + (get_transpose("integer"),
                                                '')[isinstance(i[1], str)])
            if len(i) > 2:
                have_fingering = True
                val += "-%s" % i[2]
            val += ";"
        if have_fingering:
            val = "f:1;" + val
        if val:
            return (r'%s\markup { \fret-diagram #"%s" }'
                    % (self.direction_mod(), val))
        else:
            return ''


class FretBoardNote(Music):
    def __init__(self):
        Music.__init__(self)
        self.pitch = None
        self.string = None
        self.fingering = None

    def ly_expression(self):
        s = self.pitch.ly_expression()
        if self.fingering:
            s += "-%s" % self.fingering
        if self.string:
            s += r"\%s" % self.string
        return s


class FretBoardEvent(NestedMusic):
    def __init__(self):
        NestedMusic.__init__(self)
        self.duration = None

    def print_ly(self, printer):
        fretboard_notes = [
            n for n in self.elements if isinstance(n, FretBoardNote)]
        if fretboard_notes:
            notes = []
            for n in fretboard_notes:
                notes.append(n.ly_expression())
            contents = ' '.join(notes)
            printer('<%s>%s' % (contents, self.duration))


class FunctionWrapperEvent(Event):
    def __init__(self, function_name=None):
        Event.__init__(self)
        self.function_name = function_name

    def pre_note_ly(self, is_chord_element):
        if self.function_name:
            return r"\%s" % self.function_name
        else:
            return ''

    def pre_chord_ly(self):
        return ''

    def ly_expression(self):
        if self.function_name:
            return r"\%s" % self.function_name
        else:
            return ''


class ParenthesizeEvent(FunctionWrapperEvent):
    def __init__(self):
        FunctionWrapperEvent.__init__(self, "parenthesize")


class StemEvent(Event):
    """"
    A class to take care of stem values (`up`, `down`, `double`, `none`).
    """

    def __init__(self):
        Event.__init__(self)
        self.value = None

    stem_value_dict = {
        'down': r'\once \stemDown',
        'up': r'\once \stemUp',
        'double': None,  # TODO: Implement
        'none': r'\tweak Stem.transparent ##t'
    }

    def pre_chord_ly(self):
        res = []
        value = self.stem_value_dict.get(self.value, None)
        if value is not None:
            res.append(value)
        if self.color:
            res.append(r'\tweak Stem.color #(rgb-color %s %s %s)'
                       % (self.color[0], self.color[1], self.color[2]))
        return ' '.join(res)

    def pre_note_ly(self, is_chord_element):
        return ''

    def ly_expression(self):
        return self.pre_chord_ly()


class NotestyleEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.style = None
        self.filled = None

    notehead_styles_dict = {
        'arrow down': None,  # TODO: Implement
        'arrow up': None,  # TODO: Implement
        'back slashed': None,  # TODO: Implement
        'circle dot': None,  # TODO: Implement
        'circle-x': "'xcircle",
        'circled': None,  # TODO: Implement
        'cluster': None,  # TODO: Implement
        'cross': None,  # TODO: + shaped note head
        'diamond': "'diamond",
        'do': "'do",
        'fa': "'fa",  # LilyPond automatically uses this for down-stem
        'fa up': "'fa",  # LilyPond automatically uses this for up-stem
        'inverted triangle': None,  # TODO: Implement
        'la': "'la",
        'left triangle': None,  # TODO: Implement
        'mi': "'mi",
        'none': '',
        'normal': None,
        're': "'re",
        'rectangle': None,  # TODO: Implement
        'slash': "'slash",
        'slashed': None,  # TODO: Implement
        'so': "'sol",
        'square': "'la",  # TODO: Proper squared note head
        'ti': "'ti",
        'triangle': "'triangle",
        'x': "'cross",
    }

    def pre_chord_ly(self):
        res = []
        style = self.notehead_styles_dict.get(self.style, None)
        if style == '':
            res.append(r'\tweak transparent ##t')
        elif style is not None:
            res.append(r'\tweak style #%s' % style)
        if self.color:
            res.append(r'\tweak color #(rgb-color %s %s %s)'
                       % (self.color[0], self.color[1], self.color[2]))
        return ' '.join(res)

    def pre_note_ly(self, is_chord_element):
        style = self.notehead_styles_dict.get(self.style, None)
        res = ''
        if is_chord_element:
            if style == '':
                res = r'\tweak transparent ##t'
            elif style is not None:
                res = r'\tweak style #%s' % style
        return res

    def ly_expression(self):
        return self.pre_chord_ly()


class ChordPitch(Base):
    def __init__(self):
        self.alteration = 0
        self.step = 0

    def __repr__(self):
        return self.ly_expression()

    def ly_expression(self):
        return pitch_generating_function(self)


class ChordModification(Base):
    def __init__(self):
        self.alteration = 0
        self.step = 0
        self.type = 0

    def ly_expression(self):
        if self.type:
            val = {1: ".", -1: "^"}.get(self.type, "")
            val += "%s" % self.step
            val += {1: "+", -1: "-"}.get(self.alteration, "")
            return val
        else:
            return ''


class ChordNameEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.root = None
        self.kind = None
        self.duration = None
        self.modifications = []
        self.bass = None

    def add_modification(self, mod):
        self.modifications.append(mod)

    def ly_expression(self):
        if not self.root:
            return ''
        value = self.root.ly_expression()
        if self.duration:
            value += self.duration.ly_expression()
        if self.kind:
            value = value + self.kind
        # If there are modifications, we need a `:` (plain major chords
        # don't have that).
        if self.modifications and not ":" in value:
            value += ":"
        # First print all additions and changes, then handle all
        # subtractions.
        for m in self.modifications:
            if m.type == 1:
                # Additions start with `.`, but that requires a trailing
                # digit.  If none, omit the `.`.
                if re.search(r':.*?\d$', value):
                    value += m.ly_expression()
                else:
                    value += m.ly_expression()[1:]
        for m in self.modifications:
            if m.type == -1:
                value += m.ly_expression()
        if self.bass:
            value += "/+%s" % self.bass.ly_expression()
        return value


# This is for single-stem tremolos.
class TremoloEvent(ArticulationEvent):
    def __init__(self):
        Event.__init__(self)
        self.strokes = 0

    def ly_expression(self):
        ly_str = ''
        if self.strokes and int(self.strokes) > 0:
            # `ly_dur` is a global variable defined in class `Duration`,
            # storing the current duration log value.
            #
            # * If `ly_dur` is smaller than 3, e.g., quarter, half, and whole
            #   notes, `:(2 ** (2 + number of tremolo strokes))` should be
            #   appended to the pitch and duration.  Examples:
            #
            #     1 stroke: `c4:8`, `c2:8`, or `c1:8`
            #     2 strokes: `c4:16`, `c2:16`, or `c1:16`
            #     ...
            #
            # * If `ly_dur` is equal to or greater than 3, we need to make
            #   sure that the tremolo value appended to the pitch and
            #   duration is twice the duration for a single tremolo stroke.
            #   Each additional stroke doubles the tremolo value.  Examples:
            #
            #     1 stroke: `c8:16`, `c16:32`, `c32:64`, ...
            #     2 strokes: `c8:32`, `c16:64`, `c32:128`, ...
            #     ...
            if ly_dur < 3:
                ly_str += ':%s' % (2 ** (2 + int(self.strokes)))
            else:
                ly_str += ':%s' % (2 ** (ly_dur + int(self.strokes)))
        return ly_str


class BendEvent(ArticulationEvent):
    def __init__(self):
        Event.__init__(self)
        self.alter = None

    def ly_expression(self):
        if self.alter is not None:
            return r"-\bendAfter #%s" % self.alter
        else:
            return ''


class RhythmicEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.duration = Duration()
        self.associated_events = []

    def add_associated_event(self, ev):
        if ev:
            self.associated_events.append(ev)

    def pre_chord_ly(self):
        return [ev.pre_chord_ly() for ev in self.associated_events]

    def pre_note_ly(self, is_chord_element):
        return [ev.pre_note_ly(is_chord_element)
                for ev in self.associated_events]

    def ly_expression_pre_note(self, is_chord_element):
        res = ' '.join(self.pre_note_ly(is_chord_element))
        if res != '':
            res = res + ' '
        return res

    def get_length(self, with_factor=True):
        return self.duration.get_length(with_factor)

    def get_properties(self):
        return ("'duration %s"
                % self.duration.lisp_expression())


class RestEvent(RhythmicEvent):
    def __init__(self):
        RhythmicEvent.__init__(self)
        self.pitch = None
        self.visible = True

    def ly_expression(self):
        if self.pitch:
            res = self.ly_expression_pre_note(False)
            return res + r"%s%s\rest" % (self.pitch.ly_expression(),
                                         self.duration.ly_expression())
        else:
            return 'r%s' % self.duration.ly_expression()

    def pre_note_ly(self, is_chord_element):
        if not self.visible:
            return r'\hideNote'
        else:
            return ''

    def print_ly(self, printer):
        for ev in self.associated_events:
            ev.print_ly(printer)

        if not self.visible:
            printer(r'\hideNote')

        if self.pitch:
            self.pitch.print_ly(printer)
            self.duration.print_ly(printer)
            printer.print_verbatim(r'\rest')
        else:
            printer('r')
            self.duration.print_ly(printer)


class SkipEvent(RhythmicEvent):
    def ly_expression(self):
        return 's%s' % self.duration.ly_expression()


class NoteEvent(RhythmicEvent):
    def __init__(self):
        RhythmicEvent.__init__(self)
        self.pitch = Pitch()
        self.cautionary = False
        self.editorial = False
        self.forced_accidental = False
        self.accidental_value = None
        self.visible = True

    def get_properties(self):
        s = RhythmicEvent.get_properties(self)

        if self.pitch:
            s += self.pitch.lisp_expression()

        return s

    def pitch_mods(self):
        excl_question = ''
        if self.cautionary or self.editorial:
            excl_question += '?'
        if self.forced_accidental:
            excl_question += '!'

        return excl_question

    def ly_expression(self):
        if self.pitch:
            # Obtain all stuff that needs to be printed before the note.
            res = self.ly_expression_pre_note(True)
            return res + '%s%s%s' % (self.pitch.ly_expression(),
                                     self.pitch_mods(),
                                     self.duration.ly_expression())

    def chord_element_ly(self):
        if self.pitch:
            # Obtain all stuff that needs to be printed before the note.
            res = self.ly_expression_pre_note(True)
            return res + '%s%s' % (self.pitch.ly_expression(),
                                   self.pitch_mods())

    def pre_note_ly(self, is_chord_element):
        elements = super().pre_note_ly(is_chord_element)
        if self.editorial:
            # We don't support both `editorial` and `cautionary` at the same
            # time, letting the former win.
            elements.append(r'\bracketAcc')
        if not self.visible:
            elements.append(r'\hideNote')
        return elements

    def print_ly(self, printer):
        for ev in self.associated_events:
            ev.print_ly(printer)

        color = getattr(self, 'color', None)
        if color is not None:
            printer.print_note_color("NoteHead", color)
            printer.print_note_color("Stem", color)
            printer.print_note_color("Beam", color)

        pitch = getattr(self, "pitch", None)
        if pitch is not None:
            if self.editorial:
                printer(r'\bracketAcc')
            if not self.visible:
                printer(r'\hideNote')
            pitch.print_ly(printer, self.pitch_mods())

        self.duration.print_ly(printer)


class KeySignatureChange(Music):
    def __init__(self):
        Music.__init__(self)
        self.fifths = 0
        self.tonic = None
        self.mode = 'major'
        self.non_standard_alterations = None
        self.cancel_fifths = None
        self.cancel_location = None
        self.visible = True

    def format_non_standard_alteration(self, a):
        alter_dict = {-2: ",DOUBLE-FLAT",
                      -1.5: ",THREE-Q-FLAT",
                      -1: ",FLAT",
                      -0.5: ",SEMI-FLAT",
                      0: ",NATURAL",
                      0.5: ",SEMI-SHARP",
                      1: ",SHARP",
                      1.5: ",THREE-Q-SHARP",
                      2: ",DOUBLE-SHARP"}
        try:
            accidental = alter_dict[a[1]]
        except KeyError:
            ly.warning(_("Unable to convert alteration %s "
                         "to a lilypond expression") % a[1])
            return ''
        if len(a) == 2:
            return "(%s . %s)" % (a[0], accidental)
        elif len(a) == 3:
            return "((%s . %s) . %s)" % (a[2], a[0], accidental)
        else:
            return ''

    def get_alterations(self):
        alterations = [0, 0, 0, 0, 0, 0, 0]  # for pitches C to B

        if self.non_standard_alterations:
            # `non_standard_alterations` can contain two-element or
            # three-element lists, thus the use of `*`.
            for (pitch, alteration, *dummy) in self.non_standard_alterations:
                alterations[pitch] = alteration
        else:
            count = self.fifths
            if count > 0:
                pitch = -1  # B
                while count > 0:
                    pitch = (pitch + 4) % 7  # a fifth up
                    alterations[pitch] += 1
                    count -= 1
            elif count < 0:
                pitch = 3  # F
                while count < 0:
                    pitch = (pitch - 4) % 7  # a fifth down
                    alterations[pitch] -= 1
                    count += 1
        return alterations

    def key_change_to_ly(self):
        if self.tonic:
            str = ''
            if self.cancel_fifths:
                # We ignore the value of `cancel_fifths`.
                str = {'left': r'\once \set Staff.printKeyCancellation = ##t',
                       'right': r'\cancelAfterKey',
                       'before-barline': r'\cancelBeforeBarline'
                       }.get(self.cancel_location, '')
            return (str,
                    r'\key %s \%s' % (self.tonic.ly_step_expression(),
                                      self.mode))
        elif self.non_standard_alterations:
            alterations = [self.format_non_standard_alteration(a) for
                           a in self.non_standard_alterations]
            return (r"\set Staff.keyAlterations =",
                    "#`(%s)" % " ".join(alterations))
        else:
            return ('', '')

    def ly_expression(self):
        return "%s %s" % self.key_change_to_ly()

    def print_ly(self, printer):
        (left, right) = self.key_change_to_ly()
        printer.dump(left)
        printer.dump(right)


class ShiftDurations(MusicWrapper):
    def __init__(self):
        MusicWrapper.__init__(self)
        self.params = [0, 0]

    def set_shift_durations_parameters(self, timeSigChange):
        self.params = timeSigChange.get_shift_durations_parameters()

    def print_ly(self, func):
        func(r' \shiftDurations #%d #%d ' % tuple(self.params))
        MusicWrapper.print_ly(self, func)


class TimeSignatureChange(Music):
    def __init__(self):
        Music.__init__(self)
        self.fractions = [4, 4]
        self.style = None
        # Used for the --time-signature option of musicxml2ly
        self.originalFractions = [4, 4]
        self.visible = True

    def get_fractions_ratio(self):
        """
        Calculate the ratio between the original time fraction and the new one.
        Used for the "--time-signature" option.

        @return: The ratio between the two time fractions.
        @rtype: float
        """
        return ((float(self.originalFractions[0]) / self.originalFractions[1])
                * (float(self.fractions[1]) / self.fractions[0]))

    def get_shift_durations_parameters(self):
        dur = math.ceil(math.log(self.get_fractions_ratio(), 2))
        dots = (1 / self.get_fractions_ratio()) / (math.pow(2, -dur))
        dots = int(math.log(2 - dots, 0.5))
        return [dur, dots]

    def format_fraction(self, frac):
        if isinstance(frac, list):
            l = [self.format_fraction(f) for f in frac]
            return "(" + " ".join(l) + ")"
        else:
            return "%s" % frac

    def ly_expression(self):
        st = ''
        # Print out the style if we have one, but the '() should only be
        # forced for 2/2 or 4/4, since in all other cases we'll get numeric
        # signatures anyway despite the default 'C signature style!
        is_common_signature = self.fractions in ([2, 2], [4, 4], [4, 2])
        if self.style and self.visible:
            if self.style == "common":
                st = r"\defaultTimeSignature"
            elif self.style != "'()":
                st = (r"\once \override Staff.TimeSignature.style "
                      '= #%s' % self.style)
            elif self.style != "'()" or is_common_signature:
                st = r"\numericTimeSignature"

        if self.visible:
            omit = ''
        else:
            omit = r'\omit Staff.TimeSignature'

        # Easy case: self.fractions = [n,d] => normal \time n/d call:
        if len(self.fractions) == 2 and isinstance(self.fractions[0], int):
            time = r'\time %d/%d' % tuple(self.fractions)
            return ' '.join(filter(None, [st, time, omit]))
        elif self.fractions:
            compound = (r"\compoundMeter #'%s"
                        % self.format_fraction(self.fractions))
            return ' '.join(filter(None, [st, compound, omit]))
        else:
            return st


class Clef_StaffLinesEvent(Music):
    def __init__(self):
        Music.__init__(self)
        self.type = None
        self.position = None
        self.octave = None
        self.lines = None
        self.line_details = None
        self.visible = True

    def octave_modifier(self):
        return {1: "^8", 2: "^15", -1: "_8", -2: "_15"}.get(self.octave, '')

    supported_clefs = ['G', 'F', 'C', 'percussion', 'TAB']

    lily_clef_dict = {
        ('G', 2): "treble",
        ('G', 1): "french",
        ('C', 1): "soprano",
        ('C', 2): "mezzosoprano",
        ('C', 3): "alto",
        ('C', 4): "tenor",
        ('C', 5): "baritone",
        ('F', 3): "varbaritone",
        ('F', 4): "bass",
        ('F', 5): "subbass",
        ("percussion", 2): "percussion",
        # Old MuseScore versions used `PERC` instead of `percussion`.
        ("PERC", 2): "percussion",
        ("TAB", 5): "tab",
    }

    def ly_expression(self):
        clef = None
        lines = None
        details = None

        if self.type is not None:
            if not self.position:
                # Set default position.
                self.position = {'G': 2,
                                 'F': 4,
                                 'C': 3,
                                 'percussion': 2,
                                 'PERC': 2}.get(self.type, None)

            if self.type in self.supported_clefs:
                clef_name = self.lily_clef_dict.get((self.type, self.position),
                                                    None)
                if clef_name == 'tab':
                    clef_name = get_tab_clef()
                elif not clef_name:
                    ly.warning(_("Non-standard clef positions are not "
                                 "supported yet, using 'treble' instead"))
                    clef_name = 'treble'
            else:
                if self.type == 'none':
                    # Deprecated in MusicXML version 4.0.
                    clef_name = 'treble'
                else:
                    ly.warning(_("Unsupported clef '%s', "
                                 "using 'treble' instead") % self.type)
                    clef_name = 'treble'

            clef = r'%s%s' % (clef_name, self.octave_modifier())

        if self.lines is None:
            lines = 5
        else:
            lines = self.lines

        # TODO: Also handle `line-type` and `color` attributes of the
        #       `<line-detail>` element, which unfortunately needs a much
        #       more convoluted solution because LilyPond lacks direct
        #       support (see LSR snippets #700 and #880).
        use_line_details = False
        if self.line_details is not None:
            default_lines = list(range(1, lines + 1))
            staff_lines = [e for e in default_lines
                           if e not in self.line_details
                               or self.line_details[e] != 'no']
            if staff_lines != default_lines:
                use_line_details = True
        if use_line_details:
            details = ' '.join(map(str, staff_lines))

        # LilyPond handles a 'percussion' clef similar to an alto clef; we
        # thus use `\staffLines` for this clef to get the right vertical
        # offset.
        if (clef is not None and clef != 'percussion'
                and self.lines is None and details is None):
            return r'\clef "%s"' % clef
        if clef is None:
            clef = ''
        if details is None:
            return r'\staffLines "%s" %s' % (clef, lines)
        else:
            return r'''\staffLines #'(%s) "%s" %s''' % (details, clef, lines)

    clef_dict = {
        "G": ("clefs.G", -2, -6),
        "C": ("clefs.C", 0, 0),
        "F": ("clefs.F", 2, 6),
    }

    def lisp_expression(self):
        try:
            (glyph, pos, c0) = self.clef_dict[self.type]
        except KeyError:
            return ""
        clefsetting = """
        (make-music 'SequentialMusic
        'elements (list
   (context-spec-music
   (make-property-set 'clefGlyph "%s") 'Staff)
   (context-spec-music
   (make-property-set 'clefPosition %d) 'Staff)
   (context-spec-music
   (make-property-set 'middleCPosition %d) 'Staff)))
""" % (glyph, pos, c0)
        return clefsetting


class Transposition(Music):
    def __init__(self):
        Music.__init__(self)
        self.pitch = None

    def ly_expression(self):
        self.pitch._force_absolute_pitch = True
        return r'\transposition %s' % self.pitch.ly_expression()


class StaffChange(Music):
    def __init__(self, staff):
        Music.__init__(self)
        self.staff = staff

    def ly_expression(self):
        if self.staff:
            return r'\change Staff="%s"' % self.staff
        else:
            return ''


class SetEvent(Music):
    def __init__(self, contextprop, value, once=False):
        Music.__init__(self)
        self.context_prop = contextprop
        self.value = value
        self.once = once

    def ly_expression(self):
        once_str = ''
        if self.once:
            once_str = r'\once '
        if self.value:
            return (r"%s\set %s = %s"
                    % (once_str, self.context_prop, self.value))
        else:
            return ''


class OmitEvent(Music):
    def __init__(self, contextprop, undo=False):
        Music.__init__(self)
        self.context_prop = contextprop
        self.undo = undo

    def ly_expression(self):
        undo_str = ''
        if self.undo:
            undo_str = r'\undo '
        return r'%s\omit %s' % (undo_str, self.context_prop)


class MeasureStyleEvent(Music):
    def __init__(self):
        Music.__init__(self)
        self.multiple_rest_length = 0
        self.use_symbols = False

    def ly_expression(self):
        if self.use_symbols:
            return r'\tweak expand-limit 10'
        else:
            return ''

    def print_ly(self, printer):
        s = self.ly_expression()
        if s:
            printer(s)


class TempoMark(Music):
    def __init__(self):
        Music.__init__(self)
        self.baseduration = None
        self.newduration = None
        self.bpm = None
        self.text_elements = None
        self.force_direction = None
        self.parentheses = False
        self.enclosure = None
        self.visible = True

    def wait_for_note(self):
        return False

    # Scheme function `format-metronome-markup` always uses bold face.
    # Since we don't want to redefine this function we explicitly use
    # `\normal-text` to reset the font.
    def metronome_to_ly(self):
        if (not self.visible
                or not self.baseduration
                or not (self.bpm or self.newduration)):
            if self.text_elements:
                return r'%s' % text_to_ly(self.text_elements, r'\normal-text')
            else:
                return ''

        # All the following markup gets handled within a `\concat` block.
        markup = []

        # Both Finale and MuseScore automatically insert some horizontal
        # space between the tempo and the metronome part, and we follow.
        if self.text_elements:
            elem = self.text_elements[-1][0]
            if (elem.get_name() == 'words'
                    and (elem.get_text())[-1] == ' '):
                pass
            else:
                markup.append('" "')

        markup.append(r'\normal-text \smaller {')

        if self.parentheses:
            markup.append(r'( \char ##x200A')  # U+200A HAIR SPACE
        markup.append(r'\fontsize #-2 \rhythm { %s }'
                      % self.baseduration.ly_expression())

        markup.append(r'\char ##x2009 = \char ##x2009')  # U+2009 THIN SPACE

        if self.bpm:
            markup.append(r'%s' % self.bpm)
            if self.parentheses:
                markup.append(')')
        else:
            markup.append(r'\fontsize #-2 \rhythm { %s }'
                          % self.newduration.ly_expression())
            if self.parentheses:
                markup.append(r'\char ##x200A )')

        markup.append('}')

        from musicxml import LilyPond_markup
        markup_node = LilyPond_markup()
        markup_node._data = ' '.join(markup)
        markup_attributes = {}
        if self.enclosure is not None:
            markup_attributes['enclosure'] = self.enclosure

        # We extend MusicXML by making the metronome number inherit the
        # `enclosure` attribute – or rather, we simplify the code here by
        # allowing this :-)
        text_elements = []
        if self.text_elements:
            text_elements.extend(self.text_elements)
        text_elements.append((markup_node, markup_attributes))

        return text_to_ly(text_elements, r'\normal-text')

    def print_ly(self, printer):
        dir = {-1: '#DOWN',
               1: '#UP'}.get(self.force_direction, '')
        if dir:
            override = r'\tweak direction '
            printer('%s%s' % (override, dir))

        markup = self.metronome_to_ly()
        if markup:
            printer(r'\tempo \markup')
            printer(markup)


class FiguredBassNote(Music):
    def __init__(self):
        Music.__init__(self)
        self.number = ''
        self.prefix = ''
        self.suffix = ''

    def set_prefix(self, prefix):
        self.prefix = prefix

    def set_suffix(self, suffix):
        self.prefix = suffix

    def set_number(self, number):
        self.number = number

    def ly_expression(self):
        res = ''
        if self.number:
            res += self.number
        else:
            res += '_'
        if self.prefix:
            res += self.prefix
        if self.suffix:
            res += self.suffix
        return res


class FiguredBassEvent(NestedMusic):
    def __init__(self):
        NestedMusic.__init__(self)
        self.duration = None
        self.real_duration = 0
        self.parentheses = False
        return

    def set_duration(self, dur):
        self.duration = dur

    def set_parentheses(self, par):
        self.parentheses = par

    def set_real_duration(self, dur):
        self.real_duration = dur

    def print_ly(self, printer):
        figured_bass_events = [e for e in self.elements
                               if isinstance(e, FiguredBassNote)]
        if figured_bass_events:
            notes = []
            for x in figured_bass_events:
                notes.append(x.ly_expression())
            contents = ' '.join(notes)
            if self.parentheses:
                contents = '[%s]' % contents
            printer('<%s>' % contents)
            self.duration.print_ly(printer)


class MultiMeasureRest(Music):
    def lisp_expression(self):
        return """
(make-music
  'MultiMeasureRestMusicGroup
  'elements
  (list (make-music (quote BarCheckEvent))
        (make-music
          'ChordEvent
          'elements
          (list (make-music
                  'MultiMeasureRestEvent
                  'duration
                  %s)))
        (make-music (quote BarCheckEvent))))
""" % self.duration.lisp_expression()

    def ly_expression(self):
        return 'R%s' % self.duration.ly_expression()


class Break(Music):
    def __init__(self, tp="break"):
        Music.__init__(self)
        self.type = tp

    def print_ly(self, printer):
        if self.type:
            printer.dump(r"\%s" % self.type)


class EmptyChord(Music):
    def print_ly(self, printer):
        printer.dump("<>")


class StaffGroup(Base):
    def __init__(self, command="StaffGroup"):
        self.stafftype = command
        self.id = None
        self.instrument_name = None
        self.sound = None
        self.short_instrument_name = None
        self.symbol = None
        self.spanbar = None
        self.children = []
        self.is_group = True
        self.context_modifications = []
        # part_information is a list with entries of the form
        #     [staffid, voicelist]
        # where voicelist is a list with entries of the form
        #     [voiceid1, [lyricsid11, lyricsid12,...] ]
        self.part_information = None

    def contains(self, elem):
        if self == elem:
            return True
        for c in self.children:
            if c.contains(elem):
                return True
        return False

    def append_staff(self, staff):
        self.children.append(staff)

    def find_part(self, part_id):
        # TODO: Instead of searching the tree, why not consult a score-level
        # LUT {part_id: node} that is populated as nodes are added?
        if part_id == self.id:
            return self
        for c in self.children:
            f = getattr(c, 'find_part', None)
            found = f and f(part_id)
            if found is not None:
                return found
        return None

    def set_part_information(self, staves_info):
        self.part_information = staves_info

    def add_context_modification(self, modification):
        self.context_modifications.append(modification)

    def print_ly_contents(self, printer):
        for c in self.children:
            if c:
                c.print_ly(printer)
        # Intention: I want to put the content of new StaffGroup in
        #            angled brackets (<< >>)
        # printer.dump ("test") # test is printed twice at the end of a
        #                       # staffgroup with two staves.
        # printer ("test") # test is printed twice at the end of a
        #                  # staffgroup with two staves.

    def needs_with(self):
        needs_with = False
        needs_with |= self.spanbar == "no"
        needs_with |= self.instrument_name is not None
        needs_with |= self.short_instrument_name is not None
        needs_with |= (self.symbol is not None) and (self.symbol != "bracket")
        return needs_with

    def print_ly_context_mods(self, printer):
        if self.instrument_name or self.short_instrument_name:
            printer.dump(r'\consists "Instrument_name_engraver"')
            printer.newline()
        if self.spanbar == "no":
            printer.dump(r"\hide SpanBar")
            printer.newline()
        brack = {"brace": "SystemStartBrace",
                 "none": "SystemStartBar",
                 "line": "SystemStartSquare"}.get(self.symbol, None)
        if brack:
            printer.dump("systemStartDelimiter = #'%s" % brack)
            printer.newline()

    def print_ly_overrides(self, printer):
        needs_with = self.needs_with() | (len(self.context_modifications) > 0)
        if needs_with:
            printer.dump(r"\with {")
            printer.newline()
            self.print_ly_context_mods(printer)
            for m in self.context_modifications:
                printer.dump(m)
                printer.newline()
            printer.dump("}")

    def print_chords(self, printer):
        try:
            for [staff_id, voices] in self.part_information:
                for [v, lyrics, figuredbass, chordnames, fretboards] in voices:
                    if chordnames:
                        printer(r'\context ChordNames = "%s" {' % chordnames)
                        printer.newline()
                        transpose = get_transpose("string")
                        if transpose:
                            printer.dump(transpose)
                        printer.dump(r'\%s' % chordnames)
                        printer.newline()
                        printer.dump('}')
                        printer.newline()
        except TypeError:
            return

    def print_fretboards(self, printer):
        try:
            for [staff_id, voices] in self.part_information:
                for [v, lyrics, figuredbass, chordnames, fretboards] in voices:
                    if fretboards:
                        printer(r'\context FretBoards = "%s" {' % fretboards)
                        printer.newline()
                        transpose = get_transpose("string")
                        if transpose:
                            printer.dump(transpose)
                        printer.dump(r'\%s' % fretboards)
                        printer.newline()
                        printer.dump('}')
                        printer.newline()
        except TypeError:
            return

    def print_ly(self, printer):
        self.print_chords(printer)
        self.print_fretboards(printer)
        if self.stafftype:
            printer.dump(r"\new %s" % self.stafftype)
        self.print_ly_overrides(printer)
        if self.stafftype:
            printer.dump("<<")
            printer.newline()
        if self.stafftype and self.instrument_name:
            printer.dump(r"\set %s.instrumentName =" % self.stafftype)
            printer.dump(escape_instrument_string(self.instrument_name))
            printer.newline()
        if self.stafftype and self.short_instrument_name:
            printer.dump(r"\set %s.shortInstrumentName =" % self.stafftype)
            printer.dump(escape_instrument_string(self.short_instrument_name))
            printer.newline()
        if self.sound:
            printer.dump(r'\set %s.midiInstrument = "%s"' %
                         (self.stafftype, self.sound))
            printer.newline()
        self.print_ly_contents(printer)
        if self.stafftype:
            printer.dump(">>")
            printer.newline()


class Staff(StaffGroup):
    def __init__(self, command="Staff"):
        StaffGroup.__init__(self, command)
        self.is_group = False
        self.part = None
        self.voice_command = "Voice"
        self.substafftype = None
        self.sound = None

    def needs_with(self):
        return False

    def print_ly_context_mods(self, printer):
        # printer.dump ("test") #does nothing.
        pass

    def print_ly_contents(self, printer):
        if not self.id or not self.part_information:
            return
        sub_staff_type = self.substafftype
        if not sub_staff_type:
            sub_staff_type = self.stafftype
        # printer.dump ("test") # prints test in each staff after the
        #                       # definitions of the instrument name and
        #                       # before the definition of the contexts.

        for [staff_id, voices] in self.part_information:
            # now comes the real staff definition:
            if staff_id:
                printer(r'\context %s = "%s" <<' % (sub_staff_type, staff_id))
            else:
                printer(r'\context %s <<' % sub_staff_type)
            printer.newline()
            printer.dump(r"\mergeDifferentlyDottedOn")
            printer.newline()
            printer.dump(r"\mergeDifferentlyHeadedOn")
            printer.newline()
            n = 0
            nr_voices = len(voices)
            for [v, lyrics, figuredbass, chordnames, fretboards] in voices:
                n += 1
                voice_count_text = ''
                if nr_voices > 1:
                    r"""
The next line contains a bug: The voices might not appear in numerical order!
Some voices might be missing, e.g., if the xml file contains only voice one,
three, and four, this would result in: \voiceOne, \voiceTwo, and \voiceThree.
This causes wrong stem directions and collisions.
                    """
                    voice_count_text = {
                        1: r'\voiceOne ',
                        2: r'\voiceTwo ',
                        3: r'\voiceThree '}.get(n, r'\voiceFour ')

                printer(r'\context %s = "%s" {' % (self.voice_command, v))
                printer.newline()
                transpose = get_transpose("string")
                if transpose:
                    printer.dump(transpose)
                printer.dump(r'%s\%s' % (voice_count_text, v))
                printer.newline()
                printer.dump('}')
                printer.newline()
                for (l, stanza_id) in lyrics:
                    printer(r'\new Lyrics \lyricsto "%s" {' % v)
                    printer.newline()
                    if stanza_id:
                        printer(r'\set stanza = "%s"' % stanza_id)
                    printer(r'\%s' % l)
                    printer.newline()
                    printer('}')
                    printer.newline()
                if figuredbass:
                    printer(r'\context FiguredBass = "%s" \%s' %
                            (figuredbass, figuredbass))
            printer('>>')
            printer.newline()
            # printer.dump ("test") # prints test after each definition of a
            #                       # context.
            # printer.newline ()
        # printer.dump ("test") # prints test after each definition of a
        #                       # context.

    def print_ly(self, printer):
        if self.part_information and len(self.part_information) > 1:
            self.stafftype = "PianoStaff"
            self.substafftype = "Staff"
            # printer.dump ('test')
        StaffGroup.print_ly(self, printer)


class TabStaff(Staff):
    def __init__(self, command="TabStaff"):
        Staff.__init__(self, command)
        self.string_tunings = []
        self.tablature_format = None
        self.voice_command = "TabVoice"

    def print_ly_overrides(self, printer):
        if self.string_tunings or self.tablature_format:
            printer.dump(r"\with {")
            printer.newline()
            if self.string_tunings:
                printer.dump("stringTunings = #`(")
                for i in self.string_tunings:
                    printer.dump(",%s" % i.lisp_expression())
                printer.dump(")")
                printer.newline()
            if self.tablature_format:
                printer.dump("tablatureFormat = #%s" % self.tablature_format)
                printer.newline()
            printer.dump("}")


class DrumStaff(Staff):
    def __init__(self, command="DrumStaff"):
        Staff.__init__(self, command)
        self.drum_style_table = None
        self.voice_command = "DrumVoice"

    def print_ly_overrides(self, printer):
        if self.drum_style_table:
            printer.dump(r"\with {")
            printer.dump("drumStyleTable = #%s" % self.drum_style_table)
            printer.dump("}")


class RhythmicStaff(Staff):
    def __init__(self, command="RhythmicStaff"):
        Staff.__init__(self, command)


# Test; see class Score / class Staff
# def print_staffgroup_closing_brackets (self, printer):
#       printer.dump ("test")


class Score(Base):
    def __init__(self):
        """
        Constructs a new Score object.
        """
        self.contents = None
        self.create_midi = False

    def contains(self, elem):
        if self == elem:
            return True
        if self.contents:
            return self.contents.contains(elem)
        return False

    def set_contents(self, contents):
        self.contents = contents

    def find_part(self, part_id):
        return self.contents and self.contents.find_part(part_id)

    def set_tempo(self, tempo):
        """
        Set the tempo attribute of the Score.
        This attribute can be used in L{print_ly} for the midi output
        (see L{musicxml.Sound}).

        @param tempo: The value of the tempo, in beats per minute.
        @type tempo: String
        """
        self.tempo = tempo

    # Test; see class Score / class Staff
    # def print_staffgroup_closing_brackets (self, printer):
    #     printer.dump ("test")

    def print_ly(self, printer):
        """
        Print the content of the score to the printer, in lilypond format.

        @param printer: A printer given to display correctly the output.
        @type printer: L{Output_printer<musicexp.Output_printer>}
        """
        self.create_midi = get_create_midi()
        printer.dump(r"\score {")
        printer.newline()
        # prints opening <<:
        printer.dump('<<')
        printer.newline()
        if self.contents:
            self.contents.print_ly(printer)
            # printer.dump ("test") # prints test once before the >> of the
            #                       # score block, independent of the existence
            #                       # of a staffgroup.
        # if StaffGroup == False: # True or False: nothing happens.
        #    printer.dump ('>>')
        printer.dump('>>')
        printer.newline()
        # StaffGroup.print_staffgroup_closing_brackets(self, printer)
        #   # -> TypeError: unbound method print_staffgroup_closing_brackets()
        #   #    must be called with StaffGroup instance as first argument
        #   #    (got Score instance instead)
        # print_staffgroup_closing_brackets(self, printer)
        #   # -> NameError: global name 'print_staffgroup_closing_brackets'
        #   #    is not defined.
        printer.dump(r"\layout {}")
        printer.newline()
        # If the --midi option was not passed to musicxml2ly, that comments the
        # "midi" line
        if self.create_midi:
            printer.dump("}")
            printer.newline()
            printer.dump(r"\score {")
            printer.newline()
            printer.dump(r"\unfoldRepeats \articulate {")
            printer.newline()
            self.contents.print_ly(printer)
            printer.dump("}")
            printer.newline()
        else:
            printer.dump(
                "% To create MIDI output, uncomment the following line:")
            printer.newline()
            printer.dump("%")
        printer.dump(r"\midi { \tempo 4 = " + self.tempo + " }")
        printer.newline()
        printer.dump("}")
        printer.newline()


def test_pitch():
    bflat = Pitch()
    bflat.alteration = -1
    bflat.step = 6
    bflat.octave = -1
    fifth = Pitch()
    fifth.step = 4
    down = Pitch()
    down.step = -4
    down.normalize()

    print(bflat.semitones())
    print(bflat.transposed(fifth), bflat.transposed(fifth).transposed(fifth))
    print(bflat.transposed(fifth).transposed(fifth).transposed(fifth))

    print(bflat.semitones(), 'down')
    print(bflat.transposed(down))
    print(bflat.transposed(down).transposed(down))
    print(bflat.transposed(down).transposed(down).transposed(down))


def test_printer():
    def make_note():
        evc = ChordEvent()
        n = NoteEvent()
        evc.append(n)
        return n

    def make_tup():
        m = SequentialMusic()
        m.append(make_note())
        m.append(make_note())
        m.append(make_note())

        t = TimeScaledMusic()
        t.numerator = 2
        t.denominator = 3
        t.element = m
        return t

    m = SequentialMusic()
    m.append(make_tup())
    m.append(make_tup())
    m.append(make_tup())

    printer = Output_printer()
    m.print_ly(printer)
    printer.newline()


def test_expr():
    m = SequentialMusic()
    l = 2
    evc = ChordEvent()
    n = NoteEvent()
    n.duration.duration_log = l
    n.pitch.step = 1
    evc.insert_around(None, n, 0)
    m.insert_around(None, evc, 0)

    evc = ChordEvent()
    n = NoteEvent()
    n.duration.duration_log = l
    n.pitch.step = 3
    evc.insert_around(None, n, 0)
    m.insert_around(None, evc, 0)

    evc = ChordEvent()
    n = NoteEvent()
    n.duration.duration_log = l
    n.pitch.step = 2
    evc.insert_around(None, n, 0)
    m.insert_around(None, evc, 0)

    evc = Clef_StaffLinesEvent()
    evc.type = 'treble'
    m.insert_around(None, evc, 0)

    evc = ChordEvent()
    tonic = Pitch()
    tonic.step = 2
    tonic.alteration = -2
    n = KeySignatureChange()
    n.tonic = tonic.copy()
    n.scale = [0, 0, -2, 0, 0, -2, -2]

    evc.insert_around(None, n, 0)
    m.insert_around(None, evc, 0)

    return m


if __name__ == '__main__':
    test_printer()
    test_pitch()

    expr = test_expr()
    expr.set_start(0)
    expr.print_ly(Output_printer())
    start = 0
    stop = 2

    def sub(x, start=start, stop=stop):
        ok = x.start >= start and x.start + x.get_length() <= stop
        return ok

    print(expr.lisp_sub_expression(sub))
