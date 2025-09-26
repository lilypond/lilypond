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
import math
import re
import sys
import utilities

import lilylib as ly

# Store previously converted pitch for \relative conversion as a global
# state variable
previous_pitch = None
relative_pitches = False

ly_dur = None  # For communication between `Duration` and `TremoloEvent`.


def escape_instrument_string(input_string):
    if '\\' in input_string:
        return r'\markup ' + input_string
    if input_string[0] == '"':
        return input_string
    return '"' + input_string + '"'


def color_to_ly(hex_val):
    if hex_val is None:
        return None

    # MusicXML uses ARGB notation, while LilyPond uses RGBA.
    res = re.match(r'''(?xi)
                       \# ( [0-9a-f] [0-9a-f] | )
                          ( [0-9a-f] [0-9a-f]
                            [0-9a-f] [0-9a-f]
                            [0-9a-f] [0-9a-f] ) $
                   ''', hex_val)
    if res:
        return '"#%s%s"' % res.group(2, 1)
    else:
        return None


class Output_stack_element:
    def __init__(self):
        self.factor = 1  # For scaling tuplets and the like.

    def copy(self):
        o = Output_stack_element()
        o.factor = self.factor
        return o


def split_into_paragraphs(s):
    lines = s.splitlines()
    paragraph = []

    for line in lines:
        if line.strip():
            paragraph.append(line)
        elif paragraph:
            yield ' '.join(paragraph)
            paragraph = []
    if paragraph:
        yield ' '.join(paragraph)


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
        self._nesting += 1
        self.set_nesting_strings()

        start = True
        for p in split_into_paragraphs(s):
            words = \
                utilities.split_string_and_preserve_doublequoted_substrings(p)

            if start:
                words[0] = '"' + words[0]
                start = False
            else:
                self.print_verbatim('\n')
                self.newline()

            for w in words:
                self.add_word(w)

        self.print_verbatim('"')

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
        self.repeat = 1  # For multi-measure rests.

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
        if self.repeat != 1:
            dur_str += f'*{self.repeat}'

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


# Implement command-line option `--shift-durations`.
def set_shift_durations(option):
    global shift_durations_option
    shift_durations_option = option


def get_shift_durations():
    try:
        return shift_durations_option
    except NameError:
        return 0


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
    except NameError:
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


# Implement the command-line option `--ottavas-end-early`.
def set_ottavas_end_early(option):
    global ottavas_end_early_option
    ottavas_end_early_option = option


def get_ottavas_end_early():
    try:
        if ottavas_end_early_option[0] == 't':
            return 't'
        elif ottavas_end_early_option[0] == 'f':
            return 'f'
        else:
            return 'f'
    except NameError:
        return 'f'


# Implement the command-line option `--string-numbers`.
def set_string_numbers(option):
    global string_numbers_option
    string_numbers_option = option


def get_string_numbers():
    try:
        if string_numbers_option[0] == 't':
            return 't'
        elif string_numbers_option[0] == 'f':
            return 'f'
        else:
            return 't'
    except NameError:
        return "t"


# Implement command-line option `--book`.
def set_book(option):
    global book_option
    book_option = option


def get_book():
    try:
        return book_option
    except NameError:
        return False


# Implement command-line option `--no-tagline`.
def set_tagline(option):
    global tagline_option
    tagline_option = option


def get_tagline():
    try:
        return tagline_option
    except NameError:
        return False


def generic_tone_to_pitch(tone):
    # TODO: Support different note name languages as given by the
    #       `--language` command-line option.
    accidentals_dict = {
        "": 0,
        "es": -1,
        "s": -1,
        "eses": -2,
        "ses": -2,
        "is": 1,
        "isis": 2
    }

    # TODO: `tone` might be given as a command-line argument; while the code
    #       below works just fine with ill-formed input, add a check
    #       (probably in `musicxml2ly.py`) to reject it with a user warning.
    tone_ = tone.strip().lower()
    p.octave = tone_.count("'") - tone_.count(",")
    tone_ = tone_.replace(",", "").replace("'", "")

    p = Pitch()
    p.step = (ord(tone_[0]) - ord('a') + 5) % 7
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
            return "'" * int(self.octave + 1)
        elif self.octave < -1:
            return "," * int(-self.octave - 1)
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
        self.font_size = None

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

        color = color_to_ly(self.color)
        if color is not None:
            func(r'\tweak TupletNumber.color %s' % color)
        font_size = get_font_size(self.font_size, command=False)
        if font_size is not None:
            func(r'\tweak TupletNumber.font-size %s' % font_size)

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
                            in filter(predicate, self.elements)]))

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


class SequentialMusic(NestedMusic):
    def get_last_event_chord(self):
        value = None
        at = len(self.elements) - 1
        while (at >= 0
               and not isinstance(self.elements[at], (ChordEvent, BarLine))):
            at -= 1

        if at >= 0 and isinstance(self.elements[at], ChordEvent):
            value = self.elements[at]
        return value

    def print_ly(self, printer, newline=True, closing=True):
        printer('{')
        if self.comment:
            self.print_comment(printer)

        if newline:
            printer.newline()
        for e in self.elements:
            e.print_ly(printer)

        if closing:
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


class VoiceSelector(Music):
    def __init__(self, voice):
        Music.__init__(self)
        # This code to limit the number allowed voices should stay in sync
        # with similar code in function `Staff.print_ly_contents`.
        self.voice = min(voice, 4)

    def ly_expression(self):
        return voice_text_dict[self.voice]


# This class holds attributes for both the volta number and the volta
# bracket.
class VoltaStyleEvent(Music):
    def __init__(self):
        Music.__init__(self)
        self.element = None
        self.visible = True

    def volta_style_to_ly(self):
        ret = []

        # We can't use `\tweak` here.
        text_markup = text_to_ly([self.element])
        if text_markup:
            ret.append(r'\once \override Score.VoltaBracket.text = '
                       r'\markup %s' % text_markup)
        else:
            pass  # TODO: Handle `number` attribute.

        if self.visible:
            color = color_to_ly(self.color)
            if color is not None:
                ret.append(r'\once \override Score.VoltaBracket.color = %s'
                           % color)
        else:
            ret.append(r'\once \override '
                       r'Score.VoltaBracket.transparent = ##t')

        return ret

    def ly_expression(self):
        return ' '.join(self.volta_style_to_ly())

    def print_ly(self, printer):
        for vs in self.volta_style_to_ly():
            printer(vs)


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

    def add_ending(self, volte, music):
        self.endings.append([volte, music])

    def contains(self, elem):
        return self == elem or self.music.contains(elem)

    def print_ly(self, printer):
        is_tremolo = (self.repeat_type == 'tremolo')

        if self.tremolo_strokes is not None:
            # We can't use `\tweak` here.
            printer.dump(r'\once \override Beam.gap-count = %s'
                         % self.tremolo_strokes)
        if is_tremolo and self.color is not None:
            printer.dump(r'\once \override Beam.color = %s'
                         % color_to_ly(self.color))

        if is_tremolo:
            printer(r'\repeat %s %s' % (self.repeat_type, self.repeat_count))
            if self.music:
                self.music.print_ly(printer, newline=False)
            else:
                ly.warning(_('encountered tremolo repeat without body'))
                printer('{}')
        else:
            printer(r'\repeat %s %s' % (self.repeat_type, self.repeat_count))
            if self.music:
                self.music.print_ly(printer, closing=False)
            else:
                ly.warning(_('encountered volta repeat without body'))
                printer('{')

            if self.endings:
                printer(r'\alternative {')
                printer.newline()
                for (volte, e) in self.endings:
                    printer(r'\volta %s' % ','.join(map(str, volte)))
                    e.print_ly(printer)
                printer('}')
                printer.newline()

            printer('}')
            printer.newline()


class Lyrics(Base):
    def __init__(self):
        self.lyrics_syllables = []
        self.stanza_id = None
        self.placement = None

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
                printer.print_verbatim('\n')
            else:
                printer.dump(value)
        printer.newline()

    def print_ly(self, printer):
        printer.dump(r"\header {")
        printer.newline()
        for (k, v) in self.header_fields.items():
            if v:
                self.format_header_strings(k, v, printer)
        printer.dump("}")
        printer.print_verbatim('\n')
        printer.newline()


class Paper(Base):
    def __init__(self):
        self.default_global_staff_size = 20
        self.global_staff_size = self.default_global_staff_size
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
        # staff positioning
        self.system_distance = -1
        self.top_system_distance = -1
        self.indent = 0
        self.short_indent = 0
        # other settings
        self.first_page_number = 0
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
        if (self.global_staff_size > 0
                and self.global_staff_size != self.default_global_staff_size):
            printer.dump('#(set-global-staff-size %s)' %
                         self.global_staff_size)
            printer.newline()
        printer.dump(r'\paper {')
        printer.newline()
        if self.first_page_number > 0:
            printer.dump('first-page-number = %s' % self.first_page_number)
            printer.newline()
            printer.dump('print-first-page-number = ##t')
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

        if not get_tagline():
            printer.dump('tagline = ##f')
            printer.newline()

        printer.dump('}')
        printer.print_verbatim('\n')
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
            for (context, defs) in self.context_dict.items():
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
            printer.print_verbatim('\n')
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
                if isinstance(e, (NoteEvent, RestEvent))] != []

    def get_length(self, with_factor=True):
        if self.elements:
            return max([e.get_length(with_factor) for e in self.elements])
        else:
            return 0

    def get_duration(self):
        note_events = [e for e in self.elements
                       if isinstance(e, (NoteEvent, RestEvent))]
        if note_events:
            return note_events[0].duration
        else:
            return None

    def arpeggio_pre_chord(self, printer):
        pass

    def arpeggio_post_chord(self, printer):
        pass

    def print_ly(self, printer):
        def notehead_is_changed(note_event):
            for aev in note_event.associated_events:
                if isinstance(aev, NotestyleEvent):
                    if aev.style or aev.filled is not None:
                        return True
                elif isinstance(aev, ParenthesizeEvent):
                    return True
            return False

        # Combine multiple `<fingering>` or `<pluck>` elements of a note
        # into a single fingering instruction, with its elements
        # concatenated horizontally.  An alternate fingering gets enclosed
        # in parentheses, a substitution fingering is connected with an
        # overtie to the previous fingering.
        def collect_fingerings(note_event, direction, pluck):
            fingering_events = []
            have_substitution = False
            need_markup = False

            for aev in note_event.associated_events:
                if not isinstance(aev, FingeringEvent):
                    continue

                if aev.is_pluck != pluck:
                    continue

                if not aev.visible:
                    continue

                # We collect `<fingering>` elements without `placement`
                # attribute together with elements that have
                # `placement="above"`.  Dito for `<pluck>`.
                if aev.force_direction is None:
                    force_direction = 1
                else:
                    force_direction = aev.force_direction
                if force_direction != direction:
                    continue

                # We support a single fingering substitution at the end; any
                # other fingering afterwards is ignored.
                if have_substitution:
                    aev.type = None
                    continue
                else:
                    fingering_events.append(aev)

                if aev.color or aev.font_size:
                    need_markup = True
                if aev.substitution:
                    have_substitution = True

            if not fingering_events:
                return
            fingerings = []

            if len(fingering_events) > 1 and pluck:
                need_markup = True

            if need_markup:
                if have_substitution and len(fingering_events) == 1:
                    fingerings.append('" "')

                for fev in fingering_events:
                    fingering = []

                    color = color_to_ly(fev.color)
                    if color is not None:
                        fingering.append(r'\with-color %s' % color)
                    font_size = get_font_size(fev.font_size, command=False)
                    if font_size is not None:
                        fingering.append(r'\normalsize \fontsize %s'
                                         % font_size)

                    f = fev.type
                    fev.type = None
                    if fev.alternate:
                        f = '(%s)' % f
                    fingering.append(utilities.escape_ly_output_string(f))

                    fingerings.append(' '.join(fingering))

                if have_substitution:
                    start = ' '.join(fingerings[:-2])
                    left = fingerings[-2]
                    right = fingerings[-1]
                    fingering_events[0].type = \
                        (r'\substFinger \markup \concat { %s } '
                         r'\markup %s \markup %s'
                         % (start, left, right))
                else:
                    if pluck:
                        fingering_events[0].type = \
                            (r'\RH \markup \concat { %s }'
                             % r' \char ##x200A '.join(fingerings))
                    else:
                        fingering_events[0].type = \
                            (r'\finger \markup \concat { %s }'
                             % ' '.join(fingerings))
            else:
                if have_substitution and len(fingering_events) == 1:
                    fingerings.append(' ')

                for fev in fingering_events:
                    fingering = fev.type
                    fev.type = None

                    if fev.alternate:
                        fingerings.append('(%s)' % fingering)
                    else:
                        fingerings.append(fingering)

                if have_substitution:
                    start = utilities.escape_ly_output_string(
                        ''.join(fingerings[:-2]))
                    left = utilities.escape_ly_output_string(
                        fingerings[-2])
                    right = utilities.escape_ly_output_string(
                        fingerings[-1])
                    fingering_events[0].type = (r'\substFinger %s %s %s'
                                                % (start, left, right))
                else:
                    f = ''.join(fingerings)

                    if pluck:
                        fingering_events[0].type = r'\RH "%s"' % f
                        return

                    # In the construction `<note>-<fingering>` the fingering
                    # is handled as an unsigned integer, with leading zeroes
                    # stripped off, which we don't want.
                    if f.isdigit():
                        if len(f) > 1 and f[0] == '0':
                            fingering_events[0].type = r'\finger "%s"' % f
                        else:
                            fingering_events[0].type = f
                    else:
                        fingering_events[0].type = \
                            (r'\finger %s'
                             % utilities.escape_ly_output_string(f))

        staff_changes = [e for e in self.elements
                         if isinstance(e, StaffChange)]

        note_events = [e for e in self.elements
                       if isinstance(e, NoteEvent)]

        rest_events = [e for e in self.elements
                       if isinstance(e, RhythmicEvent)
                       and not isinstance(e, NoteEvent)]

        other_events = [e for e in self.elements
                        if not isinstance(e, (RhythmicEvent, StaffChange))]

        harmonic_note_events = [e for e in note_events
                                if isinstance(e, HarmonicNoteEvent)]

        for x in note_events:
            collect_fingerings(x, -1, False)
            collect_fingerings(x, -1, True)
            collect_fingerings(x, 1, False)
            collect_fingerings(x, 1, True)

        # Depending on the `<harmonic>` elements in a chord we provide
        # default renderings in case no attributes are set that change the
        # appearance of note heads.
        #
        # We support the following combinations of harmonic-specific
        # elements (for everything else we simply use `\flageolet` symbols).
        #
        #   harmonic  natural   base-pitch touching-pitch sounding-pitch
        #   -----------------------------------------------------------------
        #      x         x          x            x                        [1]
        #      x         x                       x                        [2]
        #      x         x                       x               x        [3]
        #      x         x          x            x               x        [4]
        #
        #   harmonic artificial base-pitch touching-pitch sounding-pitch
        #   -----------------------------------------------------------------
        #      x         x          x            x                        [5]
        #      x         x          x            x               x        [4]
        #
        # [1] small black note in parentheses (base pitch)
        #     & hollow, diamond-shaped note (touching pitch)
        # [2] hollow, diamond-shaped note
        # [3] small hollow, diamond-shaped note in parentheses
        #       (touching pitch)
        #     & normal note with ring (sounding pitch)
        # [4] small black note (base pitch)
        #     & small hollow, diamond-shaped note (touching pitch)
        #     & normal note with ring (sounding pitch),
        #     & base and touching pitch are enclosed by a single pair of
        #       parentheses
        # [5] normal note (base pitch)
        #     & hollow, diamond-shaped note (touching pitch)
        #
        # Note that this algorithm fails if there are two or more harmonic
        # combinations in a single chord.  Consequently, we don't consider
        # such combinations.
        base = next((e for e in harmonic_note_events
                     if e.harmonic_type == 'base-pitch'), None)
        if (base and (not base.harmonic_visible
                      or notehead_is_changed(base))):
            base = None
        touch = next((e for e in harmonic_note_events
                      if e.harmonic_type == 'touching-pitch'), None)
        if (touch and (not touch.harmonic_visible
                       or notehead_is_changed(touch))):
            touch = None
        sound = next((e for e in harmonic_note_events
                      if e.harmonic_type == 'sounding-pitch'), None)
        if (sound and (not sound.harmonic_visible
                       or notehead_is_changed(sound))):
            sound = None

        # `harmonic_visible` is tested in
        # `HarmonicNoteEvent.harmonic_post_note_ly()` to decide whether to
        # draw a flageolet symbol.
        if len(note_events) == 3 and base and touch and sound:
            # Case 4.
            base.harmonic_steps = touch.pitch.steps() - base.pitch.steps()
            base.harmonic_type = ['small']
            base.harmonic_visible = None
            touch.harmonic_type = ['small', 'diamond']
            touch.harmonic_visible = None
        elif len(note_events) == 2:
            if base:
                if base.harmonic == 'artificial' and touch and not sound:
                    # Case 5.
                    base.harmonic_visible = None
                    touch.harmonic_type = ['diamond']
                    touch.harmonic_visible = None
                elif base.harmonic == 'natural' and touch and not sound:
                    # Case 1.
                    base.harmonic_type = ['small', 'parentheses']
                    base.harmonic_visible = None
                    touch.harmonic_type = ['diamond']
                    touch.harmonic_visible = None
            elif touch and touch.harmonic == 'natural' and sound:
                # Case 3.
                touch.harmonic_type = ['small', 'diamond', 'parentheses']
                touch.harmonic_visible = None
        elif len(note_events) == 1 and touch and touch.harmonic == 'natural':
            # Case 2.
            touch.harmonic_type = ['diamond']
            touch.harmonic_visible = None

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

        # Print all overrides and other settings for articulations or
        # ornaments that need to be inserted before the chord.
        for e in other_events:
            f = getattr(e, 'print_before_note', None)
            if f is not None:
                f(printer)

        if rest_events:
            rest_events[0].print_ly(printer)
        elif len(note_events) == 1:
            # We don't print an arpeggio line or bracket for a single note.
            note_events[0].print_ly(printer)
        elif note_events:
            global previous_pitch
            pitches = []
            basepitch = None
            stem = None
            for x in note_events:
                for aev in x.associated_events:
                    if isinstance(aev, StemEvent) and aev.value:
                        stem = aev
                pitches.append(x.chord_element_ly())
                if not basepitch:
                    basepitch = previous_pitch
            if stem:
                printer(stem.ly_expression())

            self.arpeggio_pre_chord(printer)

            printer('<%s>' % ' '.join(pitches))
            previous_pitch = basepitch
            duration = self.get_duration()
            if duration:
                duration.print_ly(printer)

            self.arpeggio_post_chord(printer)
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


class ArpeggioChordEvent(ChordEvent):
    # Don't provide a default constructor: In `musicxml2ly.py` we are not
    # creating an `ArpeggioChordEvent` object but rather converting a
    # `ChordEvent` object by changing its `__class__` attribute and thus
    # initialize the class variables manually.
    def init(self):
        self.arpeggio = None
        self.arpeggio_dir = None
        self.arpeggio_color = None
        self.arpeggio_min_pitch = 1000
        self.arpeggio_max_pitch = -1000

    def offset(self, printer):
        min_pitch = 1000
        max_pitch = -1000
        for e in self.elements:
            if isinstance(e, NoteEvent):
                min_pitch = min(min_pitch, e.pitch.steps())
                max_pitch = max(max_pitch, e.pitch.steps())

        min_offset = self.arpeggio_min_pitch - min_pitch
        max_offset = self.arpeggio_max_pitch - max_pitch
        if min_offset != 0 or max_offset != 0:
            printer(r"\offset positions #'(%s . %s)"
                    % (min_offset / 2, max_offset / 2))

    def arpeggio_pre_chord(self, printer):
        if self.arpeggio == 'non-arpeggiate':
            printer(r'\arpeggioBracket')
        elif self.arpeggio == 'arpeggiate':
            dir = {'down': r'\arpeggioArrowDown',
                   'up': r'\arpeggioArrowUp'}.get(self.arpeggio_dir, '')
            if dir:
                printer(dir)

    def arpeggio_post_chord(self, printer):
        if self.arpeggio is not None:
            color = color_to_ly(self.arpeggio_color)
            if color is not None:
                printer(r'\tweak color %s' % color)
            self.offset(printer)

            printer(r'\arpeggio')

            if (self.arpeggio == 'non-arpeggiate'
                    or self.arpeggio_dir is not None):
                printer(r'\arpeggioNormal')


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
            'tick': "'",
            'dots-heavy-heavy-dots': ':..:'}.get(self.type, None)

        val = []
        if bar_symbol is None:
            # This must be emitted before setting the color.
            val.append('|')
        if self.color is not None:
            # We can't use `\tweak` here.
            val.append(r'\once \override Staff.BarLine.color = %s'
                       % color_to_ly(self.color))
        if bar_symbol == ':..:':
            val.append(r'\once \set Score.doubleRepeatBarType = "%s"'
                       % bar_symbol)
        elif bar_symbol is not None:
            val.append(r'\bar "%s"' % bar_symbol)

        for v in val:
            printer(v)

        # Emit a comment indicating the bar number to the left.
        if self.bar_number > 1:
            printer.print_verbatim(' %% %d' % (self.bar_number - 1))
            if self.bar_number % 10 == 0:
                printer.print_verbatim('\n')
                printer.newline()
                printer(r'\barNumberCheck #%d' % self.bar_number)
        printer.newline()

    def ly_expression(self):
        return " | "


class ForBarline(Music):
    def print_ly(self, printer):
        printer(r'\forBarLine')


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
    def __init__(self, color, font_size):
        super().__init__()
        self.color = color
        self.font_size = font_size

        after_note = []
        clr = color_to_ly(color)
        if clr is not None:
            after_note.append(r'\tweak color %s' % clr)
        font_size = get_font_size(font_size, command=False)
        if font_size is not None:
            after_note.append(r'\tweak font-size %s' % font_size)
        after_note.append(r'\breathe')

        self.after_note = ' '.join(after_note)

    def ly_expression(self):
        return ''


class CaesuraEvent(Event):
    def __init__(self, color, font_size):
        super().__init__()
        self.color = color
        self.font_size = font_size

        after_note = []
        clr = color_to_ly(color)
        if clr is not None:
            after_note.append(r'\tweak color %s' % clr)
        font_size = get_font_size(font_size, command=False)
        if font_size is not None:
            after_note.append(r'\tweak font-size %s' % font_size)
        after_note.append(r'\caesura')

        self.after_note = ' '.join(after_note)

    def ly_expression(self):
        return ''


class SlurEvent(SpanEvent):
    def __init__(self):
        SpanEvent.__init__(self)
        self.number = 1  # Needed for overlapping slurs.

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
        ret = {-1: '(', 1: ')'}.get(self.span_direction, '')
        if ret and self.number != 1:
            ret = r'\=%d%s' % (self.number, ret)
        return ret

    def ly_expression(self):
        return self.slur_to_ly()

    def print_ly(self, printer):
        val = self.slur_to_ly()
        if val:
            if self.span_direction == -1:
                if self.visible:
                    color = color_to_ly(self.color)
                    if color is not None:
                        printer.dump(r'\tweak color %s' % color)
                    printer.dump('%s%s' % (self.direction_mod(), val))
                else:
                    printer.dump('%s%s%s' % (super().not_visible(),
                                             self.direction_mod(), val))
            else:
                printer.dump(val)


class BeamEvent(SpanEvent):
    def ly_expression(self):
        res = []
        if self.span_direction == -1:
            color = color_to_ly(self.color)
            if color is not None:
                res.append(r'\tweak color %s' % color)
            res.append('[')
        elif self.span_direction == 1:
            res.append(']')
        return ' '.join(res)


class PedalEvent(SpanEvent):
    def wait_for_note(self):
        if self.span_direction == 1:
            return False
        else:
            return True

    # LilyPond's support for positioning pedal marks above or below a staff
    # is limited: if there is a series of `\sustainOn` and `\sustainOff`
    # commands without any intermediate stop, all of them are positioned
    # with a single `SustainPedalLineSpanner` grob (which can span over
    # multiple systems).  In other words, positioning of single pedal marks
    # is not possible in general.  For this reason we ignore the `placement`
    # attribute.
    def ly_expression(self):
        res = []

        color = color_to_ly(self.color)
        font_size = get_font_size(self.font_size, command=False)

        if self.span_direction == 1:
            res.append('<>')
            if color is not None:
                res.append(r'\tweak color %s' % color)
            if font_size is not None:
                res.append(r'\tweak font-size %s' % font_size)

        if self.span_direction == 0 or self.span_direction == 1:
            res.append(r'\sustainOff')

        if self.span_direction == 0 or self.span_direction == -1:
            if color is not None:
                res.append(r'\tweak color %s' % color)
            if font_size is not None:
                res.append(r'\tweak font-size %s' % font_size)
            res.append(r'\sustainOn')

        return ' '.join(res)


class TextSpannerEvent(SpanEvent):
    def __init__(self):
        SpanEvent.__init__(self)
        self.text_elements = None
        self.start_stop = False  # for single-note spanners
        self.mxl_ornament = None
        self.accidental_marks = []  # for trill spanners

    def direction_mod(self):
        return {1: '^', -1: '_', 0: ''}.get(self.force_direction, '')

    def text_spanner_to_ly(self):
        if self.span_direction == 0:
            if self.accidental_marks:
                # A trill pitch change.  Being part of a spanner, we don't make
                # it inherit attributes from `<note>`.
                oe = OrnamentEvent()
                oe.type = ("", "")
                oe.accidental_marks = self.accidental_marks

                # TODO: Find a possibility to 'attach' a trill pitch change
                #       accidental mark to the spanner so that we can
                #       actually obey its `placement` attribute.  LilyPond
                #       doesn't support this currently; see issue #6724.
                #       Here we heuristically implement the most common case,
                #       which means an accidental above a trill spanner above
                #       a staff.
                dir = '^'
                ly.warning(_('Correct vertical positioning of '
                             'trill pitch change accidental marks '
                             'might fail and need manual fixing.'))

                (dummy, command, args) = oe.ly_expression()
                command = dir + command
                return ([], ' '.join([command, args]))
            else:
                return ([], '')

        ornament = None
        if self.mxl_ornament is not None:
            ornament = self.mxl_ornament
        elif self.get_paired_event().mxl_ornament is not None:
            ornament = self.get_paired_event().mxl_ornament

        val = ''
        tweaks = []

        if self.span_direction == -1:
            color = color_to_ly(self.color)
            if color is not None:
                tweaks.append(r'\tweak color %s' % color)
            font_size = get_font_size(self.font_size, command=False)
            if font_size is not None:
                tweaks.append(r'\tweak font-size %s' % font_size)

        if self.style == 'wave':
            if self.span_direction == -1:
                val = r'\startTextSpan'
                tweaks.append(r"\tweak style #'trill")

        elif self.style == "dashes":
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

        elif self.style == 'stop' and ornament is None:
            val = r'\stopTextSpan'

        else:
            if self.span_direction == -1:
                val = r'\startTrillSpan'

                spanner_color_attribute = self.color
                if spanner_color_attribute is None:
                    spanner_color_attribute = '#000000'
                trill_color_attribute = getattr(self.mxl_ornament,
                                                'color', '#000000')
                trill_color = ''
                if spanner_color_attribute != trill_color_attribute:
                    trill_color = color_to_ly(trill_color_attribute)

                trill_font_size_attribute = getattr(self.mxl_ornament,
                                                    'font-size', None)
                trill_font_size = get_font_size(trill_font_size_attribute,
                                                command=False)

                general_case = 'optional'
                if trill_color or trill_font_size:
                    general_case = 'mandatory'

                accidental_marks_command = None
                accidental_marks_args = ''
                if self.accidental_marks:
                    # Being part of a spanner, we don't make a trill pitch
                    # accidental mark inherit attributes from `<note>`.
                    oe = OrnamentEvent()
                    oe.type = ("scripts.trill", "trill")
                    oe.accidental_marks = self.accidental_marks
                    oe.force_direction = 0
                    (dummy, accidental_marks_command,
                     accidental_marks_args) = oe.ly_expression(general_case)

                if accidental_marks_command is None:
                    if trill_color or trill_font_size:
                        tweaks.append(r'\tweak bound-details.left.text '
                                      r'\markup')
                        if trill_color:
                            tweaks.append(r'\with-color %s' % trill_color)
                        if trill_font_size:
                            tweaks.append(r'\normalsize \fontsize %s'
                                          % trill_font_size)
                        tweaks.append(r'\with-true-dimension #X '
                                      r'\musicglyph "scripts.trill"')
                else:
                    if accidental_marks_command == r'\accTrill':
                        val = (r'\trillTweak %s %s' % (accidental_marks_args,
                                                       val))
                    else:
                        tweaks.append(r'\tweak bound-details.left.text '
                                      r'\markup')
                        if trill_color:
                            tweaks.append(r'\with-color %s' % trill_color)
                        if trill_font_size:
                            tweaks.append(r'\normalsize \fontsize %s'
                                          % trill_font_size)
                        tweaks.append(r'\accs-ornament %s'
                                      % accidental_marks_args)
            else:
                val = r'\stopTrillSpan'

        return (tweaks, val)

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


class DynamicsSpannerEvent(SpanEvent):
    def __init__(self):
        SpanEvent.__init__(self)
        self.text_elements = None
        self.mxl_ornament = None
        self.type = None

    def wait_for_note(self):
        if self.span_direction == -1:
            return True
        else:
            return False

    def direction_mod(self):
        return {1: '^', -1: '_', 0: ''}.get(self.force_direction, '')

    def dynamics_spanner_to_ly(self):
        init_markup = []
        init_markup.append(r'\normal-text')

        spanner_color_attribute = self.color
        if spanner_color_attribute is None:
            spanner_color_attribute = '#000000'
        dynamics_color_attribute = self.text_elements[0][1].get('color',
                                                                '#000000')

        if spanner_color_attribute != dynamics_color_attribute:
            init_markup.append(r'\with-color %s'
                               % color_to_ly(dynamics_color_attribute))

        text_markup = text_to_ly(self.text_elements, ' '.join(init_markup))

        val = ''
        tweaks = []

        if self.type == 'cresc':
            val = r'\Cresc'
        else:
            val = r'\Decresc'

        color = color_to_ly(self.color)
        if color is not None:
            tweaks.append(r'\tweak color %s' % color)
        font_size = get_font_size(self.font_size, command=False)
        if font_size is not None:
            tweaks.append(r'\tweak font-size %s' % font_size)

        if text_markup:
            tweaks.append(r'\tweak text \markup ' + text_markup)

        return (tweaks, val)

    def print_ly(self, printer):
        if self.span_direction == -1:
            (tweaks, val) = self.dynamics_spanner_to_ly()
            not_visible = super().not_visible()

            if tweaks:
                printer('%s%s' % (not_visible, tweaks[0]))
                for tweak in tweaks[1:]:
                    printer(tweak)
                printer('%s%s' % (self.direction_mod(), val))
            else:
                printer('%s%s%s' % (not_visible, self.direction_mod(), val))
        elif self.span_direction == 1:
            printer(r'<>\!')


class BracketSpannerEvent(SpanEvent):
    # Ligature brackets use prefix notation for the start.
    def wait_for_note(self):
        if self.span_direction == -1:
            return False
        else:
            return True

    def bracket_to_ly(self):
        return {1: r'\]', -1: r'\['}.get(self.span_direction, '')

    def print_ly(self, printer):
        val = self.bracket_to_ly()
        if val:
            if self.span_direction == -1:
                style = {"dashed": "dashed-line",
                         "dotted": "dotted-line",
                         "wavy": "trill"}.get(self.line_type, None)
                if style:
                    printer(r"\tweak style #'%s" % style)

                line_end_at_start = \
                    self.get_mxl_event_attribute('line-end', 'none')
                line_end_at_stop = \
                    self.get_paired_mxl_event_attribute('line-end', 'none')
                printer(r"\tweak edge-height #(make-edge-height '%s '%s)"
                        % (line_end_at_start, line_end_at_stop))

                color = color_to_ly(self.color)
                if color is not None:
                    printer(r'\tweak color %s' % color)

                dir = {1: "#UP",
                       -1: "#DOWN"}.get(self.force_direction, '')
                if dir:
                    printer(r'\tweak direction %s' % dir)

            printer(val)


class OctaveShiftEvent(SpanEvent):
    def wait_for_note(self):
        if self.span_direction == 1 and get_ottavas_end_early() == 't':
            return True
        else:
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
        value = []

        dir = self.ly_octave_shift_indicator()
        if dir:
            color = color_to_ly(self.color)
            if color is not None:
                value.append(r'\tweak color %s' % color)
            font_size = get_font_size(self.font_size, command=False)
            if font_size is not None:
                value.append(r'\tweak font-size %s' % font_size)
            value.append(r'\ottava #%s' % dir)

        return {
            -1: ' '.join(value),
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

            if self.visible:
                color = color_to_ly(self.color)
                if color is not None:
                    printer.dump(r'\once \override '
                                 r'Glissando.color = %s' % color)

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


class TieEvent(Event):
    def pre_chord_ly(self):
        return ''

    def pre_note_ly(self, is_chord_element):
        return ''

    def post_note_ly(self, is_chord_element):
        res = []
        color = color_to_ly(self.color)
        if color is not None:
            res.append(r'\tweak color %s' % color)
        res.append('~')
        return ' '.join(res)

    def ly_expression(self):
        self.post_note_ly(True)


class HairpinEvent(SpanEvent):
    def __init__(self):
        self.to_barline = False

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
                color = color_to_ly(self.color)
                if color is not None:
                    printer.dump(r'\tweak color %s' % color)
                printer.dump('%s%s' % (self.direction_mod(), val))
            else:
                pre = '<>' if self.to_barline else ''
                printer.dump('%s%s' % (pre, val))


class DynamicsEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.type = None
        self.force_direction = 0
        self.font_size_scale = 1.0
        self.to_barline = False

    def wait_for_note(self):
        return True

    def direction_mod(self):
        return {1: '^', -1: '_', 0: '-'}.get(self.force_direction, '-')

    def ly_expression(self):
        res = []
        if self.type:
            # TODO: This is a temporary solution because LilyPond ignores a
            #       dynamics symbol at the end of music with a warning.  A
            #       solution similar to handling `<offset>` is needed.
            if self.to_barline:
                res.append('<>')

            color = color_to_ly(self.color)
            if color is not None:
                res.append(r'\tweak color %s' % color)

            font_size = get_font_size(self.font_size, command=False,
                                      scale=self.font_size_scale)
            if font_size is not None:
                res.append(r'\tweak font-size %s' % font_size)

            res.append(r'%s\%s' % (self.direction_mod(), self.type))

        return ' '.join(res)


class MarkEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.text_elements = None
        self.force_direction = None

    def wait_for_note(self):
        return False

    def ly_expression(self):
        text_markup = text_to_ly(self.text_elements)
        if text_markup:
            return r'\mark \markup %s' % text_markup
        else:
            return ''

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
        text_markup = text_to_ly(self.text_elements)
        if text_markup:
            return r'\textMark \markup %s' % text_markup
        else:
            return ''

    def print_ly(self, print):
        dir = {1: '#UP',
               -1: '#DOWN'}.get(self.force_direction, '')
        if dir:
            print(r'\tweak direction %s' % dir)
        print(self.ly_expression())


# Implement command-line option `--absolute-font-sizes`.
def use_absolute_font_sizes(option):
    global absolute_font_sizes_option
    absolute_font_sizes_option = option


def get_absolute_font_sizes_option():
    try:
        return absolute_font_sizes_option
    except NameError:
        return False


def magnification_to_font_size(mag):
    return math.log2(mag) * 6


def font_size_to_magnification(size):
    return 2 ** (size / 6)


# MusicXML uses CSS-based font units, which means that 72 points equal one
# inch.  On the other hand, LilyPond uses the traditional American
# typesetting point (similar to TeX), with 72.27pt = 1in.  The former unit
# is called 'bp' ('big points') in LilyPond.
def bp_to_pt(size):
    return size * 72.27 / 72


def font_size_number_to_lily(size, ratio, command):
    size = bp_to_pt(size)
    if size > 1:
        if command and get_absolute_font_sizes_option():
            return r'\abs-fontsize #%.3f' % size
        else:
            # LilyPond uses 11pt as the default text font size.
            ref_size = 11 * ratio
            # TODO: Apply further scaling as soon as `<staff-size>` gets
            #       handled.
            scaled_size = magnification_to_font_size(size / ref_size)
            if command:
                return r'\fontsize #%.3f' % scaled_size
            else:
                return '#%.3f' % scaled_size
    else:
        return None


def font_size_word_to_lily(size, ratio, command):
    font_size_dict = {
        'xx-small': (r'\teeny', -3),
        'x-small': (r'\tiny', -2),
        'small': (r'\small', -1),
        'medium': ('', 0),
        'large': (r'\large', 1),
        'x-large': (r'\huge', 2),
        'xx-large': (r'\larger\huge', 3),
    }
    entry = font_size_dict.get(size, (None, None))

    # The comparison values are heuristic – only if the scaling doesn't
    # differ too much from LilyPond's default staff size it makes sense to
    # use commands like `\teeny` since values like 'xx-small' are still
    # absolute.
    if command and 0.9 <= ratio <= 1.1:
        return entry[0]
    else:
        unscaled_size = entry[1]
        if unscaled_size is None:
            return None

        magstep = font_size_to_magnification(unscaled_size)
        font_size = magnification_to_font_size(magstep / ratio)
        # Intentionally use low precision.
        if command:
            return r'\fontsize #%.1f' % font_size
        else:
            return '#%.1f' % font_size


def get_font_size(size, command, scale=1.0):
    if size is None:
        return None

    from musicxml2ly_globvars import paper
    ratio = paper.global_staff_size / paper.default_global_staff_size

    try:
        size = float(size)
        return font_size_number_to_lily(size * scale, ratio, command)
    except ValueError:
        return font_size_word_to_lily(size, ratio, command)


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
    # each element should get a separate enclosure by 'foo' (see
    # https://github.com/w3c/musicxml/discussions/518 and
    # https://github.com/w3c/musicxml/issues/519).  Tests with Finale and
    # MuseScore show that they do the former, and we follow.
    enclosure_attribute = elements[0][1].get('enclosure', 'none')
    enclosure = enclosure_dict.get(enclosure_attribute, '')
    if enclosure:
        # Another problem is that there is no way in MusicXML to style an
        # enclosure (see https://github.com/w3c/musicxml/issues/536).  We
        # use the first element's color attribute.
        color_attribute = elements[0][1].get('color', None)
        color = color_to_ly(color_attribute)
        if color is not None:
            markup.append(r'\with-color %s' % color)
        markup.append(enclosure)
    prev_enclosure = enclosure

    if init_markup is not None:
        markup.append(init_markup)

    closing_braces = 0
    if elements[0][0].get_name() == 'lilypond-markup' or len(elements) > 1:
        markup.append(r'\concat {')
        closing_braces += 1

    for (element, attributes) in elements:
        enclosure_attribute = attributes.get('enclosure', 'none')
        enclosure = enclosure_dict.get(enclosure_attribute, '')
        if prev_enclosure != enclosure:
            if enclosure:
                markup.append(enclosure)
            markup.append(r'\concat {')
            closing_braces += 1

            prev_enclosure = enclosure

        font_size_attribute = attributes.get('font-size', '')
        font_size_scale = attributes.get('font-size-scale', 1.0)
        font_size = get_font_size(font_size_attribute, command=True,
                                  scale=font_size_scale)
        if font_size is not None:
            markup.append(font_size)

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

        color_attribute = attributes.get('color', None)
        color = color_to_ly(color_attribute)
        if color is not None:
            markup.append(r'\with-color %s' % color)

        text = ''
        name = element.get_name()
        if name in ['credit-words',
                    'display-text',
                    'ending',
                    'group-abbreviation',
                    'group-name',
                    'part-abbreviation',
                    'part-name',
                    'rehearsal',
                    'words']:
            text = element.get_text()
            if text:
                # For whitespace handling, if the `xml:space` attribute is
                # set to 'default' (or not explicitly specified), W3C
                # recommends to apply the normalization algorithm of
                # attribute values also for the content of XML elements:
                # remove leading and trailing whitespace, convert all
                # whitespace to normal spaces, and finally collapse a
                # sequence of spaces to a single space.
                #
                # Note that not all programs do this while importing
                # MusicXML.  Using `<words>` as an example, MuseScore 4.4
                # ignores its `xml:space` attribute, always assuming the
                # value 'preserve'.  On the other hand, Finale 27.4 also
                # ignores `xml:space`, does not collapse spaces, but applies
                # a special treatment to collapse CR and LF.
                #
                # Since (at least) these two programs also export MusicXML
                # with meaningful leading and trailing whitespace without
                # setting `xml:space` to 'preserve', we follow.
                if attributes.get('xml:space', 'default') != 'preserve':
                    # We use Python's special algorithm of `split()`, which
                    # kicks in if there is no separator argument, to
                    # eliminate runs of consecutive whitespace characters.
                    # We temporarily add guards to get this also for leading
                    # and trailing whitespace.
                    text = '|' + text + '|'
                    text = ' '.join(text.split())
                    text = text[1:-1]
                    text = utilities.escape_ly_output_string(text)
                else:
                    # `\r` can only be created with `&#xD;`.
                    lines = text.replace('\r', '\n').split('\n')
                    if len(lines) > 1:
                        text = []
                        text.append(r'\center-column {')
                        for line in lines:
                            if line:
                                text.append(
                                    utilities.escape_ly_output_string(line))
                            else:
                                text.append(r'\null')
                        text.append('}')
                        text = ' '.join(text)
                    else:
                        text = utilities.escape_ly_output_string(lines[0])
        elif name == 'segno':
            text = r'\fontsize #2 \segno'
        elif name == 'coda':
            text = r'\fontsize #2 \coda'
        elif name == 'accidental-text':
            accidental = accidental_values_dict.get(element.get_text(), None)
            if accidental is not None:
                if accidental.isascii():
                    text = r'\tiny \musicglyph "%s"' % accidental
                else:
                    text = r'\tiny \number "%s"' % accidental
        elif name == 'lilypond-markup':
            text = element.get_text()
        else:
            pass  # XXX

        if text:
            markup.append(text)

    for _ in range(closing_braces):
        markup.append('}')

    return ' '.join(markup)


class TextEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.text_elements = None
        self.force_direction = None
        self.to_barline = False

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
        text_markup = text_to_ly(self.text_elements)
        if text_markup:
            # TODO: This is a temporary solution because text at the end of
            #       music silently disappears.  A solution similar to
            #       handling `<offset>` is needed.
            pre = '<>' if self.to_barline else ''
            return r'%s%s\markup %s' % (pre, self.direction_mod(), text_markup)
        else:
            return ''


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
        res = []
        color = color_to_ly(self.color)
        if color is not None:
            res.append(r'\tweak color %s' % color)
        font_size = get_font_size(self.font_size, command=False)
        if font_size is not None:
            res.append(r'\tweak font-size %s' % font_size)

        res.append(r'%s\%s' % (self.direction_mod(), self.type))

        return ' '.join(res)


# The Unicode-encoded accidentals select text variants in the Emmentaler
# font.  In the code we use `isascii()` to select whether `\number` or
# `\musicglyph` gets used.
accidental_values_dict = {
    'sharp': '♯',
    'natural': '♮',
    'flat': '♭',
    'double-sharp': '𝄪',
    'sharp-sharp': '♯♯',
    'flat-flat': '𝄫',
    'natural-sharp': '♮♯',
    'natural-flat': '♮♭',
    'quarter-flat': 'accidentals.mirroredflat',
    'quarter-sharp': 'accidentals.sharp.slashslash.stem',
    'three-quarters-flat': 'accidentals.mirroredflat.flat',
    'three-quarters-sharp': 'accidentals.sharp.slashslash.stemstemstem',
    'sharp-down': 'accidentals.sharp.arrowdown',
    'sharp-up': 'accidentals.sharp.arrowup',
    'natural-down': 'accidentals.natural.arrowdown',
    'natural-up': 'accidentals.natural.arrowup',
    'flat-down': 'accidentals.flat.arrowdown',
    'flat-up': 'accidentals.flat.arrowup',
    # 'double-sharp-down': '???',
    # 'double-sharp-up': '???',
    # 'flat-flat-down': '???',
    # 'flat-flat-up': '???',
    # 'arrow-up': '???',
    # 'arrow-down': '???',
    'triple-sharp': '♯𝄪',
    'triple-flat': '♭𝄫',
    'slash-quarter-sharp': 'accidentals.sharp.slashslashslash.stem',
    'slash-sharp': 'accidentals.sharp.slashslashslash.stemstem',
    'slash-flat': 'accidentals.flat.slash',
    'double-slash-flat': 'accidentals.flat.slashslash',
    # 'sharp-1': '???',
    # 'sharp-2': '???',
    # 'sharp-3': '???',
    # 'sharp-5': '???',
    # 'flat-1': '???',
    # 'flat-2': '???',
    # 'flat-3': '???',
    # 'flat-4': '???',
    'sori': 'accidentals.sharp.sori',
    'koron': 'accidentals.flat.koron',
    # 'other': '???',
}


class OrnamentEvent(ArticulationEvent):
    def __init__(self):
        ArticulationEvent.__init__(self)
        self.note_color = None
        self.note_font_size = None
        self.accidental_marks = []
        self.force_direction = 1

    def ly_expression(self, general_case=False):
        tweaks = []
        command = None
        args = ''

        color = color_to_ly(self.color)
        if color is not None:
            tweaks.append(r'\tweak color %s' % color)
        else:
            color = '"#000000"'

        font_size = get_font_size(self.font_size, command=False)
        if font_size is not None:
            tweaks.append(r'\tweak font-size %s' % font_size)

        # `<accidental-mark>` elements get mapped to the LilyPond markup
        # command `\ornament` for 'simple' cases (i.e., there is at most one
        # accidental mark above and below that both have the same color as
        # the ornament, and there is no `font-size` attribute for the two
        # accidental marks) or `\accs-ornament` for the general case.  If
        # there are no accidental marks at all, use LilyPond's standard
        # ornament command.
        #
        # For trills, we use `\accTrill` if there is a single accidental mark
        # above the trill.
        #
        # If `general_case` is set to 'optional', only emit `\accTrill` or
        # `\accs-ornament`; if set to 'mandatory', only emit
        # `\accs-ornament`.

        above_marks = []
        below_marks = []
        same_color = True
        have_font_size = False

        for a in self.accidental_marks:
            a_value = accidental_values_dict.get(a.get_text(), None)
            if a_value is None:
                continue

            # Color and font size gets inherited from `<note>` (if not part of a
            # spanner).
            a_color = color_to_ly(getattr(a, 'color', self.note_color))
            a_font_size = get_font_size(getattr(a, 'font-size',
                                                self.note_font_size),
                                        command=False)
            a_placement = getattr(a, 'placement', 'above')

            # Similar to normal accidentals, give brackets precedence over
            # parentheses.
            a_bracket = getattr(a, 'bracket', 'no')
            a_parentheses = getattr(a, 'parentheses', 'no')
            a_enclosure = None
            if a_bracket == 'yes':
                a_enclosure = '[]'
            elif a_parentheses == 'yes':
                a_enclosure = '()'

            if a_color is None:
                a_color = '"#000000"'

            a_mark = (a_value, a_color, a_font_size, a_enclosure)
            if a_placement == 'above':
                above_marks.append(a_mark)
            else:
                below_marks.append(a_mark)

            if a_color != color:
                same_color = False
            if a_font_size is not None:
                have_font_size = True

        if (same_color and not have_font_size
                and len(above_marks) <= 1 and len(below_marks) <= 1
                and general_case != 'mandatory'
                and not (general_case == 'optional' and below_marks)):
            # The 'simple' case.
            above = ''
            if above_marks:
                (a_value, dummy, dummy, a_enclosure) = above_marks[0]
                above = a_value
                if a_enclosure:
                    above = a_enclosure[0] + above + a_enclosure[1]

            below = ''
            if below_marks:
                (b_value, dummy, dummy, b_enclosure) = below_marks[0]
                below = b_value
                if b_enclosure:
                    below = b_enclosure[0] + below + b_enclosure[1]

            if above or below:
                if self.type[1] == 'trill' and not below:
                    command = r'\accTrill'
                    args = '"%s"' % above
                else:
                    # This case doesn't happen for trill spanners.
                    command = r'\ornament'
                    args = r'"%s" "%s" "%s"' % (above, self.type[0], below)
            else:
                # This case doesn't happen for trill spanners.
                command = r'\%s' % self.type[1]
        else:
            # The general case.
            above = []
            below = []
            for (marks, acc) in [(above_marks, above), (below_marks, below)]:
                for (a_value, a_color, a_font_size, a_enclosure) in marks:
                    markup = []
                    if color != '"#000000"' or a_color != '"#000000"':
                        markup.append(r'\with-color %s' % a_color)
                    if a_font_size is not None:
                        markup.append(r'\normalsize \fontsize %s'
                                      % a_font_size)

                    if a_value.isascii():
                        m = r'\musicglyph "%s"' % a_value
                    else:
                        m = r'\number "%s"' % a_value

                    if a_enclosure:
                        # We increase the font size of the enclosure by three
                        # magnitudes (see `acc-font-size` and
                        # `enclosure-font-size` properties of the
                        # `\accs-ornament` markup command).  This is a bit
                        # awkward since we have to hard-code it here.  Also be
                        # careful not to change the precision.
                        delta = 3
                        if a_font_size:
                            e = a_font_size[1:].split('.')
                            enc_font_size = str(int(e[0]) + delta)
                            if len(e) > 1:
                                enc_font_size += e[1]

                            command = (r'\normalsize \fontsize #%s'
                                       % enc_font_size)
                        else:
                            command = r'\fontsize #%s' % delta

                        m = (r'\concat { %s "%s" %s %s "%s" }'
                             % (command, a_enclosure[0],
                                m,
                                command, a_enclosure[1]))

                    markup.append(m)

                    acc.append(' '.join(markup))

            if self.type[0]:
                ornament = r'\musicglyph "%s"' % self.type[0]
            else:
                ornament = '##f'

            tweaks.append(r'\tweak parent-alignment-X #CENTER')
            tweaks.append(r'\tweak self-alignment-X #CENTER')
            command = r'\markup \accs-ornament'
            args = (r'{ %s } %s { %s }'
                    % (' '.join(above), ornament, ' '.join(below)))

        return (tweaks, command, args)

    def print_ly(self, printer):
        dir = self.direction_mod()
        (tweaks, command, args) = self.ly_expression()

        for t in tweaks:
            printer(t)
        printer("%s%s" % (dir, command))
        printer(args)


class ShortArticulationEvent(ArticulationEvent):
    def direction_mod(self):
        # default is -
        return {1: '^', -1: '_', 0: '-'}.get(self.force_direction, '-')

    def ly_expression(self):
        res = []
        if self.type:
            color = color_to_ly(self.color)
            if color is not None:
                res.append(r'\tweak color %s' % color)
            font_size = get_font_size(self.font_size, command=False)
            if font_size is not None:
                res.append(r'\tweak font-size %s' % font_size)

            res.append(r'%s%s' % (self.direction_mod(), self.type))

        return ' '.join(res)


class NoDirectionArticulationEvent(ArticulationEvent):
    def ly_expression(self):
        res = []
        if self.type:
            color = color_to_ly(self.color)
            if color is not None:
                res.append(r'\tweak color %s' % color)
            font_size = get_font_size(self.font_size, command=False)
            if font_size is not None:
                res.append(r'\tweak font-size %s' % font_size)

            res.append(r'\%s' % self.type)

        return ' '.join(res)


class FingeringEvent(ShortArticulationEvent):
    def __init__(self):
        ArticulationEvent.__init__(self)
        self.is_pluck = False
        self.alternate = False
        self.substitution = False
        self.visible = True

    def pre_chord_ly(self):
        return ''

    def pre_note_ly(self, is_chord_element):
        return ''

    def post_note_ly(self, is_chord_element):
        res = []
        if self.type:
            # Color and font size gets handled by `ChordEvent.print_ly`.
            res.append(r'%s%s' % (self.direction_mod(), self.type))

        return ' '.join(res)

    def ly_expression(self):
        self.post_note_ly(True)


class MarkupEvent(ShortArticulationEvent):
    def __init__(self):
        ArticulationEvent.__init__(self)
        self.contents = None

    def ly_expression(self):
        res = []
        if self.contents:
            color = color_to_ly(self.color)
            if color is not None:
                res.append(r'\tweak color %s' % color)
            font_size = get_font_size(self.font_size, command=False)
            if font_size is not None:
                res.append(r'\tweak font-size %s' % font_size)

            res.append(r"%s\markup { %s }"
                       % (self.direction_mod(), self.contents))

        return ' '.join(res)


class AccidentalMarkEvent(MarkupEvent):
    def __init__(self):
        MarkupEvent.__init__(self)
        self.force_direction = 1

    def ly_expression(self):
        contents = accidental_values_dict.get(self.contents, None)
        if contents is None:
            return ''

        if contents.isascii():
            contents = r'\musicglyph "%s"' % contents
        else:
            contents = r'\number "%s"' % contents

        res = []

        # Accidental marks should be horizontally centered on the note
        # head.
        res.append(r'\tweak parent-alignment-X #CENTER')
        res.append(r'\tweak self-alignment-X #CENTER')

        color = color_to_ly(self.color)
        if color is not None:
            res.append(r'\tweak color %s' % color)
        font_size = get_font_size(self.font_size, command=False)
        if font_size is not None:
            res.append(r'\tweak font-size %s' % font_size)

        res.append(r"%s\markup %s" % (self.direction_mod(), contents))

        return ' '.join(res)


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

    def post_note_ly(self, is_chord_element):
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
        self.is_stemlet = False

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

        if value != 'none':
            color = color_to_ly(self.color)
            if color is not None:
                res.append(r'\tweak Stem.color %s' % color)

        if self.is_stemlet:
            res.append(r'\tweak Stem.stemlet-length #1')

        return ' '.join(res)

    def pre_note_ly(self, is_chord_element):
        return ''

    def post_note_ly(self, is_chord_element):
        return ''

    def ly_expression(self):
        return self.pre_chord_ly()


class NotestyleEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.duration = None
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
        'diamond': "'harmonic-mixed",
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
        'triangle': "'do",
        'x': "'cross",
    }

    def pre_chord_ly(self):
        return ''

    def pre_note_ly(self, is_chord_element):
        res = []

        if is_chord_element:
            style = self.notehead_styles_dict.get(self.style, None)
            if style == '':
                res.append(r'\tweak transparent ##t')
            elif style is not None:
                res.append(r'\tweak style #%s' % style)

            if style != '':
                if self.duration < 0.5 and self.filled == 'no':
                    res.append(r'\tweak duration-log #1')
                elif self.duration >= 0.5 and self.filled == 'yes':
                    res.append(r'\tweak duration-log #2')

                color = color_to_ly(self.color)
                if color is not None:
                    res.append(r'\tweak color %s' % color)
                font_size = get_font_size(self.font_size, command=False)
                if font_size is not None:
                    res.append(r'\tweak font-size %s' % font_size)

        return ' '.join(res)

    def post_note_ly(self, is_chord_element):
        return ''

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
        if self.modifications and ":" not in value:
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
        res = []
        if self.strokes and int(self.strokes) > 0:
            color = color_to_ly(self.color)
            if color is not None:
                res.append(r'\tweak color %s' % color)

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
                res.append(':%s' % (2 ** (2 + int(self.strokes))))
            else:
                res.append(':%s' % (2 ** (ly_dur + int(self.strokes))))

        return ' '.join(res)


class BendEvent(ArticulationEvent):
    def __init__(self):
        Event.__init__(self)
        self.alter = None

    def ly_expression(self):
        res = []
        if self.alter is not None:
            color = color_to_ly(self.color)
            if color is not None:
                res.append(r'\tweak color %s' % color)

            res.append(r'-\bendAfter #%s' % self.alter)

        return ' '.join(res)


class RhythmicEvent(Event):
    def __init__(self):
        Event.__init__(self)
        self.duration = Duration()
        self.dot_color = None
        self.dot_font_size = None
        # 'Associated events' are tightly connected with a (LilyPond) note
        # or rest.  Examples are stems or notehead styles.  Adjusting their
        # attributes must work within chords, too.  Classes that are used as
        # associated events must provide three functions:
        #
        #   pre_chord_ly: return string of items to be added before a chord
        #                 (or a single note)
        #   pre_note_ly: return string of items to be added right before a
        #                note
        #   post_note_ly: return string of items to be added right after a
        #                note
        self.associated_events = []

    def add_associated_event(self, ev):
        if ev:
            self.associated_events.append(ev)

    def pre_chord_ly(self):
        return [ev.pre_chord_ly() for ev in self.associated_events]

    def pre_note_ly(self, is_chord_element):
        return [ev.pre_note_ly(is_chord_element)
                for ev in self.associated_events]

    def post_note_ly(self, is_chord_element):
        return [ev.post_note_ly(is_chord_element)
                for ev in self.associated_events]

    def ly_expression_pre_chord(self):
        return ' '.join(filter(None, self.pre_chord_ly()))

    def ly_expression_pre_note(self, is_chord_element):
        return ' '.join(filter(None, self.pre_note_ly(is_chord_element)))

    def ly_expression_post_note(self, is_chord_element):
        return ' '.join(filter(None, self.post_note_ly(is_chord_element)))

    def get_length(self, with_factor=True):
        return self.duration.get_length(with_factor)

    def get_properties(self):
        return ("'duration %s"
                % self.duration.lisp_expression())


class RestEvent(RhythmicEvent):
    def __init__(self):
        RhythmicEvent.__init__(self)
        self.pitch = None
        self.y_offset = 0  # For pitched full-measure rests.
        self.full_measure_glyph = False
        self.visible = True
        self.spacing = True

    def pre_note_ly(self, is_chord_element):
        elements = super().pre_note_ly(is_chord_element)

        if not self.visible:
            elements.append(r'\hideNote')
        else:
            color = color_to_ly(self.color)
            if color is not None:
                elements.append(r'\tweak color %s' % color)

            font_size = get_font_size(self.font_size, command=False)
            if font_size is not None:
                # See issue #6721 why we currently need this work-around.
                if self.duration.dots:
                    elements.insert(0, r'\once \override Rest.font-size = %s'
                                    % font_size)
                else:
                    elements.append(r'\tweak font-size %s' % font_size)

            dot_color = color_to_ly(self.dot_color)
            if dot_color is not None:
                elements.append(r'\tweak Dots.color %s' % dot_color)
            dot_font_size = get_font_size(self.dot_font_size, command=False)
            if dot_font_size is not None:
                # See issue #6721 why we currently need this work-around.
                # res.append(r'\tweak Dots.font-size %s' % dot_font_size)
                elements.insert(0, r'\once \override Dots.font-size = %s'
                                % font_size)

            if self.y_offset != 0:
                elements.append(r'\tweak Y-offset #%s' % self.y_offset)

        return elements

    def post_note_ly(self, is_chord_element):
        elements = super().post_note_ly(is_chord_element)
        return elements

    def print_ly(self, printer):
        if self.duration is None:
            return

        printer(self.ly_expression_pre_chord())
        printer(self.ly_expression_pre_note(True))

        if self.pitch:
            self.pitch.print_ly(printer)
            self.duration.print_ly(printer)
            printer.print_verbatim(r'\rest')
        else:
            if self.full_measure_glyph:
                printer('R')
            else:
                printer('r')
            self.duration.print_ly(printer)

        printer(self.ly_expression_post_note(True))


class SkipEvent(RhythmicEvent):
    def __init__(self):
        RhythmicEvent.__init__(self)
        self.grace_skip = None

    def ly_expression(self):
        res = []

        if self.grace_skip is not None:
            res.append(r'\grace { s%s }' % self.grace_skip.ly_expression())
        res.append('s%s' % self.duration.ly_expression())

        return ' '.join(res)


class NoteEvent(RhythmicEvent):
    def __init__(self):
        RhythmicEvent.__init__(self)
        self.pitch = Pitch()
        self.cautionary = False
        self.editorial = False
        self.forced_accidental = False
        self.accidental_value = None
        self.accidental_color = None
        self.accidental_font_size = None
        self.visible = True
        self.spacing = True

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
            # Obtain all stuff that needs to be printed before and after the
            # note.
            res = []
            res.append(self.ly_expression_pre_note(True))
            res.append('%s%s%s' % (self.pitch.ly_expression(),
                                   self.pitch_mods(),
                                   self.duration.ly_expression()))
            res.append(self.ly_expression_post_note(True))
            return ' '.join(filter(None, res))

    def chord_element_ly(self):
        if self.pitch:
            # Obtain all stuff that needs to be printed before and after the
            # note.
            res = []
            res.append(self.ly_expression_pre_note(True))
            res.append('%s%s' % (self.pitch.ly_expression(),
                                 self.pitch_mods()))
            res.append(self.ly_expression_post_note(True))
            return ' '.join(filter(None, res))

    def harmonic_pre_note_ly(self, elements):
        pass

    def harmonic_post_note_ly(self, elements):
        pass

    def pre_note_ly(self, is_chord_element):
        elements = super().pre_note_ly(is_chord_element)
        if self.editorial:
            # We don't support both `editorial` and `cautionary` at the same
            # time, letting the former win.
            elements.append(r'\bracketAcc')

        if not self.visible:
            elements.append(r'\hideNote')
        else:
            accidental_color = color_to_ly(self.accidental_color)
            if accidental_color is not None:
                elements.append(r'\tweak Accidental.color %s'
                                % accidental_color)
            accidental_font_size = get_font_size(self.accidental_font_size,
                                                 command=False)
            if accidental_font_size is not None:
                elements.append(r'\tweak Accidental.font-size %s'
                                % accidental_font_size)

            dot_color = color_to_ly(self.dot_color)
            if dot_color is not None:
                elements.append(r'\tweak Dots.color %s' % dot_color)
            dot_font_size = get_font_size(self.dot_font_size, command=False)
            if dot_font_size is not None:
                elements.append(r'\tweak Dots.font-size %s' % dot_font_size)

            self.harmonic_pre_note_ly(elements)

        return elements

    def post_note_ly(self, is_chord_element):
        elements = super().post_note_ly(is_chord_element)

        self.harmonic_post_note_ly(elements)

        return elements

    def print_ly(self, printer):
        if self.spacing:
            pitch = getattr(self, "pitch", None)
            if pitch is not None:
                printer(self.ly_expression_pre_chord())
                printer(self.ly_expression_pre_note(True))
                pitch.print_ly(printer, self.pitch_mods())
        else:
            # We completely ignore objects without spacing; their purpose is
            # not typesetting but providing better MIDI support.
            printer('s')

        self.duration.print_ly(printer)
        printer(self.ly_expression_post_note(True))


class HarmonicNoteEvent(NoteEvent):
    # Don't provide a default constructor: In `musicxml.py` we are not
    # creating a `HarmonicNoteEvent` object but rather converting a
    # `NoteEvent` object by changing its `__class__` attribute and thus
    # initialize the class variables manually.
    def init(self):
        self.harmonic = None
        self.harmonic_type = None
        self.harmonic_visible = None
        self.harmonic_color = None
        self.harmonic_font_size = None
        self.harmonic_steps = None  # between base pitch and touching pitch

    # Return a heuristic size and a vertical offset for Emmentaler's
    # parentheses to enclose two notes in a chord with a separation given by
    # `harmonic_steps`.
    def harmonic_parentheses_tweaks(self):
        return (-3.8 + abs(self.harmonic_steps) * 1.4,
                self.harmonic_steps / 4)

    def harmonic_pre_note_ly(self, elements):
        if self.harmonic_steps:
            elements.append(r'\harmonicParen %.1f %.1f'
                            % self.harmonic_parentheses_tweaks())

        if isinstance(self.harmonic_type, list):
            if 'small' in self.harmonic_type:
                elements.append(r'\harmonicSmall')
            if 'parentheses' in self.harmonic_type:
                elements.append(r'\parenthesize')

    def harmonic_post_note_ly(self, elements):
        if isinstance(self.harmonic_type, list):
            if 'diamond' in self.harmonic_type:
                elements.append(r'\harmonic')
        elif self.harmonic_visible:
            # A circular harmonic symbol.
            harmonic_color = color_to_ly(self.harmonic_color)
            if harmonic_color is not None:
                elements.append(r'\tweak color %s' % harmonic_color)
            harmonic_font_size = get_font_size(self.harmonic_font_size,
                                               command=False)
            if harmonic_font_size is not None:
                elements.append(r'\tweak font-size %s' % harmonic_font_size)
            elements.append(r'\flageolet')


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
        color = color_to_ly(self.color)
        font_size = get_font_size(self.font_size, command=False)

        if self.tonic:
            str = ''
            if self.cancel_fifths:
                # We ignore the value of `cancel_fifths`.
                str = {'left': r'\once \set Staff.printKeyCancellation = ##t',
                       'right': r'\cancelAfterKey',
                       'before-barline': r'\cancelBeforeBarline'
                       }.get(self.cancel_location, '')

            if color is None:
                color_tweak = ''
            else:
                color_tweak = r'\tweak color %s ' % color

            if font_size is None:
                font_size_tweak = ''
            else:
                font_size_tweak = r'\tweak font-size %s ' % font_size

            return (str,
                    r'%s%s\key %s \%s' % (color_tweak,
                                          font_size_tweak,
                                          self.tonic.ly_step_expression(),
                                          self.mode))
        elif self.non_standard_alterations:
            alterations = [self.format_non_standard_alteration(a) for
                           a in self.non_standard_alterations]

            if color is None:
                color_override = ''
            else:
                color_override = (r' \once \override '
                                  r'Staff.KeySignature.color = %s' % color)

            if font_size is None:
                font_size_override = ''
            else:
                font_size_override = (r' \once \override '
                                      r'Staff.KeySignature.font-size = %s'
                                      % font_size)

            return (r'\set Staff.keyAlterations =',
                    r'#`(%s)%s%s' % (' '.join(alterations),
                                     color_override,
                                     font_size_override))
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

    def print_ly(self, func):
        func(r' \shiftDurations %d 0' % get_shift_durations())
        MusicWrapper.print_ly(self, func)


class TimeSignatureChange(Music):
    def __init__(self):
        Music.__init__(self)
        self.fractions = [4, 4]
        self.style = None
        self.visible = True

    def format_fraction(self, frac):
        if isinstance(frac, list):
            l = [self.format_fraction(f) for f in frac]
            return "(" + " ".join(l) + ")"
        else:
            return "%s" % frac

    def ly_expression(self):
        ret = []

        # Print out the style if we have one, but the '() should only be
        # forced for 2/2 or 4/4, since in all other cases we'll get numeric
        # signatures anyway despite the default 'C signature style!
        if self.visible:
            if self.style:
                is_common_signature = \
                    self.fractions in ([2, 2], [4, 4], [4, 2])

                if self.style == "common":
                    ret.append(r'\defaultTimeSignature')
                elif self.style != "'()":
                    ret.append(r'\once \override Staff.TimeSignature.style '
                               r'= #%s' % self.style)
                elif self.style != "'()" or is_common_signature:
                    ret.append(r'\numericTimeSignature')

            color = color_to_ly(self.color)
            if color is not None:
                ret.append(r'\once \override Staff.TimeSignature.color '
                           r'= %s' % color)

            font_size = get_font_size(self.font_size, command=False)
            if font_size is not None:
                ret.append(r'\once \override Staff.TimeSignature.font-size '
                           r'= %s' % font_size)
        else:
            ret.append(r'\once \omit Staff.TimeSignature')

        # Easy case: self.fractions = [n,d] => normal \time n/d call:
        if len(self.fractions) == 2 and isinstance(self.fractions[0], int):
            ret.append(r'\time %d/%d' % tuple(self.fractions))
        elif self.fractions:
            ret.append(r"\compoundMeter #'%s"
                       % self.format_fraction(self.fractions))

        return ' '.join(ret)


class Clef_StaffLinesEvent(Music):
    def __init__(self):
        Music.__init__(self)
        self.type = None
        self.position = None
        self.pitch = None  # The pitch of the middle line (if applicable).
        self.octave = None
        self.lines = None
        self.line_details = None
        self.lines_color = None
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

        color = color_to_ly(self.color)
        font_size = get_font_size(self.font_size, command=False)

        # LilyPond handles a 'percussion' clef similar to an alto clef; we
        # thus use `\staffLines` for this clef to get the right vertical
        # offset.
        if (clef is not None and clef != 'percussion'
                and self.lines is None and details is None):
            ret = []

            if color is not None:
                # We can't use `\tweak` here.
                ret.append(r'\once \override Staff.Clef.color = %s' % color)
            if font_size is not None:
                ret.append(r'\once \override Staff.Clef.font-size = %s'
                           % font_size)

            ret.append(r'\clef "%s"' % clef)
            return ' '.join(ret)

        if clef is None:
            clef = ''

        lines_color = color_to_ly(self.lines_color)

        properties = []
        if details is not None:
            properties.append('(details . (%s))' % details)
        if color is not None:
            properties.append('(clef-color . %s)' % color)
        if font_size is not None:
            # Skip the leading `#` character in `font_size`.
            properties.append('(clef-font-size . %s)' % font_size[1:])
        if lines_color is not None:
            properties.append('(staff-color . %s)' % lines_color)

        props = ' '.join(properties)
        if props:
            return r'''\staffLines #'(%s) "%s" %s''' % (props, clef, lines)
        else:
            return r'\staffLines "%s" %s' % (clef, lines)

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
        ret = []

        if self.use_symbols:
            ret.append(r'\tweak expand-limit 10')

        color = color_to_ly(self.color)
        if color is not None:
            ret.append(r'\tweak color %s' % color)
            ret.append(r'\tweak MultiMeasureRestNumber.color %s' % color)
        font_size = get_font_size(self.font_size, command=False)
        if font_size is not None:
            ret.append(r'\tweak font-size %s' % font_size)
            ret.append(r'\tweak MultiMeasureRestNumber.font-size %s'
                       % font_size)

        return ' '.join(ret)

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


system_start_dict = {
    'brace': 'SystemStartBrace',
    'bracket': None,  # default of `systemStartDelimiter` in `StaffGroup`
    'line': None,  # TODO: Implement
    'none': 'SystemStartBar',
    'square': 'SystemStartSquare',
}


class StaffGroup(Base):
    def __init__(self, command="StaffGroup"):
        self.stafftype = command
        self.id = None
        self.instrument_name = None
        self.sound = None
        self.short_instrument_name = None
        self.symbol = None
        self.spanbar = 'no'
        self.connect_barlines = False
        self.children = []
        self.is_group = True
        self.is_last_staff = False
        self.context_modifications = []
        # See the comment before function `format_staff_info` together with
        # function `update_score_setup` (both in file `musicxml2ly.py`) how
        # entries look like.
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
        have_group_instrument_name = False
        if (self.instrument_name is not None
                or self.short_instrument_name is not None):
            have_group_instrument_name = True

        last_child = self.children[-1]
        for c in self.children:
            if c:
                if c == last_child:
                    # The bar line connection of a group's bottommost staff
                    # depends on the parent's value.
                    c.connect_barlines = self.connect_barlines
                else:
                    # Only if the parent's value isn't set to 'True' we
                    # check `spanbar`.
                    if self.connect_barlines:
                        c.connect_barlines = True
                    else:
                        c.connect_barlines = (self.spanbar != 'no')

                if not c.is_group and have_group_instrument_name:
                    c.have_group_instrument_name = True
                c.print_ly(printer)

        # Intention: I want to put the content of new StaffGroup in
        #            angled brackets (<< >>)
        # printer.dump ("test") # test is printed twice at the end of a
        #                       # staffgroup with two staves.
        # printer ("test") # test is printed twice at the end of a
        #                  # staffgroup with two staves.

    def needs_with(self):
        needs_with = False
        needs_with |= self.spanbar == 'Mensurstrich'
        needs_with |= self.instrument_name is not None
        needs_with |= self.short_instrument_name is not None
        needs_with |= (self.symbol is not None) and (self.symbol != "bracket")
        return needs_with

    def print_ly_context_mods(self, printer):
        if self.instrument_name or self.short_instrument_name:
            printer.dump(r'\consists "Instrument_name_engraver"')
            printer.newline()
        if self.spanbar == 'Mensurstrich':
            printer('measureBarType = "-span|"')
            printer.newline()
        brack = system_start_dict.get(self.symbol, None)
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
            for [_, voices] in self.part_information:
                for [v, lyrics, figuredbass, chordnames, fretboards] in voices:
                    if chordnames:
                        printer(r'\context ChordNames = "%s"' % chordnames)
                        transpose = get_transpose("string")
                        if transpose:
                            printer.dump(transpose)
                        printer.dump('{')
                        printer.newline()
                        printer.dump(r'\%s' % chordnames)
                        printer.newline()
                        printer.dump('}')
                        printer.newline()
        except TypeError:
            return

    def print_fretboards(self, printer):
        try:
            for [_, voices] in self.part_information:
                for [v, lyrics, figuredbass, chordnames, fretboards] in voices:
                    if fretboards:
                        printer(r'\context FretBoards = "%s"' % fretboards)
                        transpose = get_transpose("string")
                        if transpose:
                            printer.dump(transpose)
                        printer.dump('{')
                        printer.newline()
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
            if isinstance(self, Staff) and self.stafftype != 'PianoStaff':
                printer.dump(r'\new %s = "%s"' % (self.stafftype, self.id))
            else:
                printer.dump(r'\new %s' % self.stafftype)
        self.print_ly_overrides(printer)
        if self.stafftype:
            printer("<<")
            printer.newline()
            if self.instrument_name:
                printer(r"\set %s.instrumentName =" % self.stafftype)
                printer(escape_instrument_string(self.instrument_name))
                printer.newline()
            if self.short_instrument_name:
                printer(r"\set %s.shortInstrumentName =" % self.stafftype)
                printer(escape_instrument_string(self.short_instrument_name))
                printer.newline()
            if not self.is_group and self.have_group_instrument_name:
                printer(r"\override Staff.InstrumentName.self-alignment-X = "
                        r"#RIGHT")
                printer.newline()
                printer(r"\override Staff.InstrumentName.padding = #1")
                printer.newline()

        if self.sound:
            printer.dump(r'\set %s.midiInstrument = "%s"' %
                         (self.stafftype, self.sound))
            printer.newline()
        self.print_ly_contents(printer)
        if self.stafftype:
            printer.dump(">>")
            printer.newline()


voice_text_dict = {
    0: r'\oneVoice',
    1: r'\voiceOne',
    2: r'\voiceTwo',
    3: r'\voiceThree',
    4: r'\voiceFour',
}


class Staff(StaffGroup):
    def __init__(self, command="Staff"):
        StaffGroup.__init__(self, command)
        self.is_group = False
        self.part = None
        # default of `systemStartDelimiter` in `PianoStaff`
        self.part_symbol = 'brace'
        self.barline_top = 0
        self.barline_bottom = 0
        self.have_group_instrument_name = False
        self.voice_command = "Voice"
        self.substafftype = None
        self.sound = None

    def needs_with(self):
        return False

    def print_ly_context_mods(self, printer):
        pass

    def print_ly_contents(self, printer):
        if not self.id or not self.part_information:
            return
        sub_staff_type = self.substafftype
        if not sub_staff_type:
            sub_staff_type = self.stafftype

        top = self.barline_top
        bottom = self.barline_bottom
        part_len = len(self.part_information)
        # Ignore invalid range.
        if top > part_len or bottom > part_len or bottom < top:
            top = bottom = 0
        # Normalize.
        if top == 0 and bottom == part_len:
            bottom = 0

        for i, [staff_id, voices] in enumerate(self.part_information, start=1):
            if i == top:
                printer(r'\new PianoStaff')
                if self.part_symbol != 'brace':
                    brack = system_start_dict.get(self.part_symbol, None)
                    if brack:
                        printer(r'\with {')
                        printer.newline()
                        printer("systemStartDelimiter = #'%s" % brack)
                        printer.newline()
                        printer('}')
                printer('<<')
                printer.newline()

            # Now comes the real definition of a part's staff (or staves).
            if staff_id:
                printer(r'\context %s = "%s" <<' % (sub_staff_type, staff_id))
            else:
                printer(r'\context %s <<' % sub_staff_type)
            printer.newline()

            # Check whether we have to interrupt the bar line connection.
            # There is nothing to do if all bar lines are to be connected
            # anyway, or if we are at the bottommost staff.
            if not (self.connect_barlines
                    or (self.is_last_staff and i == part_len)):
                # Within a part, interrupt the bar line connection before
                # and after the staves (if any) that have the staff group
                # delimiter, and after the last staff.
                if (((top or bottom) and not top <= i < bottom)
                        or i == part_len):
                    printer(r'\override Staff.BarLine.allow-span-bar = ##f')
                    printer.newline()

            printer.dump(r"\mergeDifferentlyDottedOn")
            printer.newline()
            printer.dump(r"\mergeDifferentlyHeadedOn")
            printer.newline()
            n = 0
            nr_voices = len(voices)

            voice_warning = False

            for [v, lyrics, figuredbass, chordnames, fretboards] in voices:
                n += 1
                voice_text = ''
                if nr_voices > 1:
                    # TODO: Support more voices.
                    voice_number = n
                    if voice_number > 4:
                        if not voice_warning:
                            ly.warning(_('Only up to 4 voices per staff are '
                                         'supported; expect wrong stem '
                                         'directions and collisions'))
                            voice_warning = True
                        voice_number = 4

                    # TODO: Voices might not appear in LilyPond order, i.e.,
                    #       some voices might be missing!  For example, if
                    #       the MusicXML file contains only voices one,
                    #       three, and four (in LilyPond order), this
                    #       currently still results in `\voiceOne`,
                    #       `\voiceTwo`, and `\voiceThree`, causing wrong
                    #       stem directions and possibly collisions.
                    #
                    #       A solution might be to add some heuristics while
                    #       mapping MusicXML voices to LilyPond voices,
                    #       checking stem directions (irrespective of option
                    #       `--no-stem-directions`).  Unfortunately, this
                    #       might fail spectacularly, especially in piano
                    #       music with its ad-hoc polyphony, where there is
                    #       no guarantee that the voice order stays the same
                    #       during the whole piece.
                    #
                    #       To better support piano music and the like a
                    #       completely different paradigm would be necessary,
                    #       also using ad-hoc polyphony on the LilyPond side
                    #       (i.e., replacing global voices with local
                    #       `<<...\\...>>` constructs).
                    voice_text = '%s ' % voice_text_dict[voice_number]

                printer(r'\context %s = "%s"' % (self.voice_command, v))
                transpose = get_transpose("string")
                if transpose:
                    printer.dump(transpose)
                printer.dump('{')
                printer.newline()
                printer.dump(r'%s\%s' % (voice_text, v))
                printer.newline()
                printer.dump('}')
                printer.newline()
                for (l, stanza_id, placement) in lyrics:
                    printer(r'\new Lyrics')
                    if placement == 'above':
                        printer(r'\with {')
                        printer.newline()
                        if self.stafftype == 'PianoStaff':
                            id = staff_id
                        else:
                            id = self.id
                        printer('alignAboveContext = "%s"' % id)
                        printer.newline()
                        printer('}')
                    printer(r'\lyricsto "%s" {' % v)
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

            if i == bottom:
                printer('>>')
                printer.newline()

    def print_ly(self, printer):
        if self.part_information and len(self.part_information) > 1:
            # The group delimiter and the bar line connection for a
            # multi-staff part are not controlled by `<group-barline>` but by
            # `<part-symbol>`.
            if self.barline_top or self.barline_bottom:
                self.symbol = 'none'
            else:
                self.symbol = self.part_symbol

            self.stafftype = "PianoStaff"
            self.substafftype = "Staff"
            # printer.dump ('test')

        if self.symbol != 'brace':
            brack = system_start_dict.get(self.symbol, None)
            if brack:
                self.add_context_modification("systemStartDelimiter = #'%s"
                                              % brack)

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
        if get_book():
            printer.dump(r'\book {')
            printer.newline()
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
        if get_book():
            printer.dump('}')
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
