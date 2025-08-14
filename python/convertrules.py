# -*- coding: utf-8 -*-
# (setq py-indent-offset 4)
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 1998--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>,
#                          Jan Nieuwenhuizen <janneke@gnu.org>
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
import re
import string
import sys

import lilylib


NOT_SMART = "\n" + _("Not smart enough to convert %s.") + "\n"
UPDATE_MANUALLY = _(
    "Please refer to the manual for details, and update manually.") + "\n"
FROM_TO = _("%s has been replaced by %s") + "\n"


class FatalConversionError(Exception):
    pass


conversions = []
stderr_write = sys.stderr.write


def warning(s):
    stderr_write(_("warning: %s") % s)

# Decorator to make rule syntax simpler


def rule(version, message):
    """
    version: a LilyPond version tuple like (2, 11, 50)
    message: the message that describes the conversion.

    This decorator adds its function together with the version and the
    message to the global conversions list.  (It doesn't need to return
    the function as it isn't used directly anyway.)

    A conversion rule using this decorator looks like this:

    @rule ((1, 2, 3), "convert foo to bar")
    def conv(s):
        s = s.replace('foo', 'bar')
        return s

    """
    def dec(f):
        conversions.append((version, f, message))
    return dec


@rule((0, 1, 9), r'\header { key = concat + with + operator }')
def conv(s):
    if re.search('\\\\multi', s):
        stderr_write(NOT_SMART % "\\multi")
    return s


@rule((0, 1, 19), r'deprecated \octave')
def conv(s):
    if re.search('\\\\octave', s):
        stderr_write(NOT_SMART % "\\octave")
        stderr_write(UPDATE_MANUALLY)
    #   raise FatalConversionError ()
    return s


@rule((0, 1, 20), r'deprecated \textstyle, new \key syntax')
def conv(s):
    s = re.sub('\\\\textstyle([^;]+);',
                 '\\\\property Lyrics . textstyle = \\1', s)
    # harmful to current .lys
    # s = re.sub ('\\\\key([^;]+);', '\\\\accidentals \\1;', s)
    return s


@rule((0, 1, 21), r'\musical_pitch -> \musicalpitch, \meter -> \time')
def conv(s):
    s = re.sub('\\\\musical_pitch', '\\\\musicalpitch', s)
    s = re.sub('\\\\meter', '\\\\time', s)
    return s


@rule((1, 0, 0), "bump version for release")
def conv(s):
    return s


@rule((1, 0, 1), r'\accidentals -> \keysignature, specialaccidentals -> keyoctaviation')
def conv(s):
    s = re.sub('\\\\accidentals', '\\\\keysignature', s)
    s = re.sub('specialaccidentals *= *1', 'keyoctaviation = 0', s)
    s = re.sub('specialaccidentals *= *0', 'keyoctaviation = 1', s)
    return s


@rule((1, 0, 2), r'\header { key = concat + with + operator }')
def conv(s):
    if re.search('\\\\header', s):
        stderr_write(NOT_SMART % _("new \\header format"))
    return s


@rule((1, 0, 3), r'\melodic -> \notes')
def conv(s):
    s = re.sub('\\\\melodic([^a-zA-Z])', '\\\\notes\\1', s)
    return s


@rule((1, 0, 4), 'default_{paper,midi}')
def conv(s):
    s = re.sub('default_paper *=', '', s)
    s = re.sub('default_midi *=', '', s)
    return s


@rule((1, 0, 5), 'ChoireStaff -> ChoirStaff')
def conv(s):
    s = re.sub('ChoireStaff', 'ChoirStaff', s)
    s = re.sub('\\\\output', 'output = ', s)
    return s


@rule((1, 0, 6), r'foo = \translator {\type .. } -> \translator {\type ..; foo; }')
def conv(s):
    if re.search('[a-zA-Z]+ = *\\translator', s):
        stderr_write(NOT_SMART % _("\\translator syntax"))
    #   raise FatalConversionError ()
    return s


@rule((1, 0, 7), r'\lyric -> \lyrics')
def conv(s):
    s = re.sub('\\\\lyrics*', '\\\\lyrics', s)
    return s


@rule((1, 0, 10), r'[2/3 ]1/1 -> \times 2/3 ')
def conv(s):
    s = re.sub('\\\\\\[/3+', '\\\\times 2/3 { ', s)
    s = re.sub('\\[/3+', '\\\\times 2/3 { [', s)
    s = re.sub('\\\\\\[([0-9/]+)', '\\\\times \\1 {', s)
    s = re.sub('\\[([0-9/]+)', '\\\\times \\1 { [', s)
    s = re.sub('\\\\\\]([0-9/]+)', '}', s)
    s = re.sub('\\\\\\]', '}', s)
    s = re.sub('\\]([0-9/]+)', '] }', s)
    return s


@rule((1, 0, 12), 'Chord syntax stuff')
def conv(s):
    return s


@rule((1, 0, 13), '<a ~ b> c -> <a b> ~ c')
def conv(s):
    s = re.sub('<([^>~]+)~([^>]*)>', '<\\1 \\2> ~', s)
    return s


@rule((1, 0, 14), '<[a b> <a b]>c -> [<a b> <a b>]')
def conv(s):
    s = re.sub('<\\[', '[<', s)
    s = re.sub('\\]>', '>]', s)
    return s


@rule((1, 0, 16), r'\type -> \context, textstyle -> textStyle')
def conv(s):
    s = re.sub('\\\\type([^\n]*engraver)', '\\\\TYPE\\1', s)
    s = re.sub('\\\\type([^\n]*performer)', '\\\\TYPE\\1', s)
    s = re.sub('\\\\type', '\\\\context', s)
    s = re.sub('\\\\TYPE', '\\\\type', s)
    s = re.sub('textstyle', 'textStyle', s)
    return s


@rule((1, 0, 18), r'\repeat NUM Music Alternative -> \repeat FOLDSTR Music Alternative')
def conv(s):
    if re.search('\\\\repeat', s):
        stderr_write(NOT_SMART % "\\repeat")
    #   raise FatalConversionError ()
    return s


@rule((1, 0, 19), 'fontsize -> fontSize, midi_instrument -> midiInstrument, SkipBars -> skipBars')
def conv(s):
    s = re.sub('SkipBars', 'skipBars', s)
    s = re.sub('fontsize', 'fontSize', s)
    s = re.sub('midi_instrument', 'midiInstrument', s)
    return s


@rule((1, 0, 20), '{,tie,slur}ydirection -> {v,tieV,slurV}erticalDirection')
def conv(s):
    s = re.sub('tieydirection', 'tieVerticalDirection', s)
    s = re.sub('slurydirection', 'slurVerticalDirection', s)
    s = re.sub('ydirection', 'verticalDirection', s)
    return s


@rule((1, 0, 21), 'hshift -> horizontalNoteShift')
def conv(s):
    s = re.sub('hshift', 'horizontalNoteShift', s)
    return s


@rule((1, 1, 52), r'deprecate \grouping')
def conv(s):
    s = re.sub('\\\\grouping[^;]*;', '', s)
    return s


@rule((1, 1, 55), r'\wheel -> \coda')
def conv(s):
    s = re.sub('\\\\wheel', '\\\\coda', s)
    return s


@rule((1, 1, 65), 'slurdash -> slurDash, keyoctaviation -> keyOctaviation')
def conv(s):
    s = re.sub('keyoctaviation', 'keyOctaviation', s)
    s = re.sub('slurdash', 'slurDash', s)
    return s


@rule((1, 1, 66), 'semi -> volta')
def conv(s):
    s = re.sub('\\\\repeat *"?semi"?', '\\\\repeat "volta"', s)
    return s


@rule((1, 1, 67), 'beamAuto -> noAutoBeaming')
def conv(s):
    s = re.sub('"?beamAuto"? *= *"?0?"?', 'noAutoBeaming = "1"', s)
    return s


@rule((1, 2, 0), 'automaticMelismas -> automaticMelismata')
def conv(s):
    s = re.sub('automaticMelismas', 'automaticMelismata', s)
    return s


@rule((1, 2, 1), 'dynamicDir -> dynamicDirection')
def conv(s):
    s = re.sub('dynamicDir\\b', 'dynamicDirection', s)
    return s


@rule((1, 3, 4), r'\cadenza -> \cadenza{On|Off}')
def conv(s):
    s = re.sub('\\\\cadenza *0 *;', '\\\\cadenzaOff', s)
    s = re.sub('\\\\cadenza *1 *;', '\\\\cadenzaOn', s)
    return s


@rule((1, 3, 5), 'beamAuto moment properties')
def conv(s):
    s = re.sub('"?beamAuto([^"=]+)"? *= *"([0-9]+)/([0-9]+)" *;*',
                 'beamAuto\\1 = #(make-moment \\2 \\3)',
                 s)
    return s


@rule((1, 3, 17), 'stemStyle -> flagStyle')
def conv(s):
    s = re.sub('stemStyle',
                 'flagStyle',
                 s)
    return s


@rule((1, 3, 18), 'staffLineLeading -> staffSpace')
def conv(s):
    s = re.sub('staffLineLeading',
                 'staffSpace',
                 s)
    return s


@rule((1, 3, 23), r'deprecate \repetitions')
def conv(s):
    if re.search('\\\\repetitions', s):
        stderr_write(NOT_SMART % "\\repetitions")
    #   raise FatalConversionError ()
    return s


@rule((1, 3, 35), 'textEmptyDimension -> textNonEmpty')
def conv(s):
    s = re.sub('textEmptyDimension *= *##t',
                 'textNonEmpty = ##f',
                 s)
    s = re.sub('textEmptyDimension *= *##f',
                 'textNonEmpty = ##t',
                 s)
    return s


@rule((1, 3, 38), r"\musicalpitch { a b c } -> #'(a b c)")
def conv(s):
    s = re.sub("([a-z]+)[ \t]*=[ \t]*\\\\musicalpitch *{([- 0-9]+)} *\n",
                 "(\\1 . (\\2))\n", s)
    s = re.sub("\\\\musicalpitch *{([0-9 -]+)}",
                 "\\\\musicalpitch #'(\\1)", s)
    if re.search('\\\\notenames', s):
        stderr_write(NOT_SMART % _("new \\notenames format"))
    return s


@rule((1, 3, 39), r'\key A ;  ->\key a;')
def conv(s):
    def replace(match):
        return '\\key %s;' % match.group(1).lower()

    s = re.sub("\\\\key ([^;]+);",  replace, s)
    return s


@rule((1, 3, 41), r'[:16 c4 d4 ] -> \repeat "tremolo" 2 { c16 d16 }')
def conv(s):
    if re.search('\\[:', s):
        stderr_write(NOT_SMART % _("new tremolo format"))
    return s


@rule((1, 3, 42), 'Staff_margin_engraver deprecated, use Instrument_name_engraver')
def conv(s):
    s = re.sub('Staff_margin_engraver', 'Instrument_name_engraver', s)
    return s


@rule((1, 3, 49), 'noteHeadStyle value: string -> symbol')
def conv(s):
    s = re.sub('note[hH]eadStyle\\s*=\\s*"?(\\w+)"?',
                 "noteHeadStyle = #'\\1", s)
    return s


@rule((1, 3, 58), 'noteHeadStyle value: string -> symbol')
def conv(s):
    if re.search('\\\\keysignature', s):
        stderr_write(NOT_SMART % '\\keysignature')
    return s


@rule((1, 3, 59), r'\key X ; -> \key X major; ')
def conv(s):
    s = re.sub(r"""\\key *([a-z]+) *;""", r"""\\key \1 \\major;""", s)
    return s


@rule((1, 3, 68), r'latexheaders = "\input global" -> latexheaders = "global"')
def conv(s):
    s = re.sub(r'latexheaders *= *"\\\\input ',
                 'latexheaders = "',
                 s)
    return s


# TODO: lots of other syntax changes should be done here as well
@rule((1, 3, 92), 'basicXXXProperties -> XXX, Repeat_engraver -> Volta_engraver')
def conv(s):
    s = re.sub('basicCollisionProperties', 'NoteCollision', s)
    s = re.sub('basicVoltaSpannerProperties', "VoltaBracket", s)
    s = re.sub('basicKeyProperties', "KeySignature", s)

    s = re.sub('basicClefItemProperties', "Clef", s)

    s = re.sub('basicLocalKeyProperties', "Accidentals", s)
    s = re.sub('basicMarkProperties', "Accidentals", s)
    s = re.sub('basic([A-Za-z_]+)Properties', '\\1', s)

    s = re.sub('Repeat_engraver', 'Volta_engraver', s)
    return s


@rule((1, 3, 93), 'change property definition case (eg. onevoice -> oneVoice)')
def conv(s):
    # Ugh, but meaning of \stemup changed too
    # maybe we should do \stemup -> \stemUp\slurUp\tieUp ?
    s = re.sub('\\\\stemup', '\\\\stemUp', s)
    s = re.sub('\\\\stemdown', '\\\\stemDown', s)
    s = re.sub('\\\\stemboth', '\\\\stemBoth', s)

    s = re.sub('\\\\slurup', '\\\\slurUp', s)
    s = re.sub('\\\\slurboth', '\\\\slurBoth', s)
    s = re.sub('\\\\slurdown', '\\\\slurDown', s)
    s = re.sub('\\\\slurdotted', '\\\\slurDotted', s)
    s = re.sub('\\\\slurnormal', '\\\\slurNoDots', s)

    s = re.sub('\\\\shiftoff', '\\\\shiftOff', s)
    s = re.sub('\\\\shifton', '\\\\shiftOn', s)
    s = re.sub('\\\\shiftonn', '\\\\shiftOnn', s)
    s = re.sub('\\\\shiftonnn', '\\\\shiftOnnn', s)

    s = re.sub('\\\\onevoice', '\\\\oneVoice', s)
    s = re.sub('\\\\voiceone', '\\\\voiceOne', s)
    s = re.sub('\\\\voicetwo', '\\\\voiceTwo', s)
    s = re.sub('\\\\voicethree', '\\\\voiceThree', s)
    s = re.sub('\\\\voicefour', '\\\\voiceFour', s)

    # I don't know exactly when these happened...
    # ugh, we lose context setting here...
    s = re.sub('\\\\property *[^ ]*verticalDirection[^=]*= *#?"?(1|(\\\\up))"?',
                 '\\\\stemUp\\\\slurUp\\\\tieUp', s)
    s = re.sub('\\\\property *[^ ]*verticalDirection[^=]*= *#?"?((-1)|(\\\\down))"?',
                 '\\\\stemDown\\\\slurDown\\\\tieDown', s)
    s = re.sub('\\\\property *[^ ]*verticalDirection[^=]*= *#?"?(0|(\\\\center))"?',
                 '\\\\stemBoth\\\\slurBoth\\\\tieBoth', s)

    s = re.sub('verticalDirection[^=]*= *#?"?(1|(\\\\up))"?',
                 'Stem \\\\override #\'direction = #0\nSlur \\\\override #\'direction = #0\n Tie \\\\override #\'direction = #1', s)
    s = re.sub('verticalDirection[^=]*= *#?"?((-1)|(\\\\down))"?',
                 'Stem \\\\override #\'direction = #0\nSlur \\\\override #\'direction = #0\n Tie \\\\override #\'direction = #-1', s)
    s = re.sub('verticalDirection[^=]*= *#?"?(0|(\\\\center))"?',
                 'Stem \\\\override #\'direction = #0\nSlur \\\\override #\'direction = #0\n Tie \\\\override #\'direction = #0', s)

    s = re.sub(
        '\\\\property *[^ .]*[.]?([a-z]+)VerticalDirection[^=]*= *#?"?(1|(\\\\up))"?', '\\\\\\1Up', s)
    s = re.sub(
        '\\\\property *[^ .]*[.]?([a-z]+)VerticalDirection[^=]*= *#?"?((-1)|(\\\\down))"?', '\\\\\\1Down', s)
    s = re.sub(
        '\\\\property *[^ .]*[.]?([a-z]+)VerticalDirection[^=]*= *#?"?(0|(\\\\center))"?', '\\\\\\1Both', s)

    # (lacks capitalization slur -> Slur)
    s = re.sub('([a-z]+)VerticalDirection[^=]*= *#?"?(1|(\\\\up))"?',
                 '\\1 \\\\override #\'direction = #1', s)
    s = re.sub('([a-z]+)VerticalDirection[^=]*= *#?"?((-1)|(\\\\down))"?',
                 '\\1 \\\\override #\'direction = #-1', s)
    s = re.sub('([a-z]+)VerticalDirection[^=]*= *#?"?(0|(\\\\center))"?',
                 '\\1 \\\\override #\'direction = #0', s)

    # dynamic..
    s = re.sub(
        '\\\\property *[^ .]*[.]?dynamicDirection[^=]*= *#?"?(1|(\\\\up))"?', '\\\\dynamicUp', s)
    s = re.sub(
        '\\\\property *[^ .]*[.]?dyn[^=]*= *#?"?((-1)|(\\\\down))"?', '\\\\dynamicDown', s)
    s = re.sub(
        '\\\\property *[^ .]*[.]?dyn[^=]*= *#?"?(0|(\\\\center))"?', '\\\\dynamicBoth', s)

    s = re.sub(
        '\\\\property *[^ .]*[.]?([a-z]+)Dash[^=]*= *#?"?(0|(""))"?', '\\\\\\1NoDots', s)
    s = re.sub(
        '\\\\property *[^ .]*[.]?([a-z]+)Dash[^=]*= *#?"?([1-9]+)"?', '\\\\\\1Dotted', s)

    s = re.sub(
        '\\\\property *[^ .]*[.]?noAutoBeaming[^=]*= *#?"?(0|(""))"?', '\\\\autoBeamOn', s)
    s = re.sub(
        '\\\\property *[^ .]*[.]?noAutoBeaming[^=]*= *#?"?([1-9]+)"?', '\\\\autoBeamOff', s)
    return s


@rule((1, 3, 97), 'ChordName -> ChordNames')
def conv(s):
    s = re.sub('ChordNames*', 'ChordNames', s)
    if re.search('\\\\textscript "[^"]* *"[^"]*"', s):
        stderr_write(NOT_SMART % _("new \\textscript markup text"))

    s = re.sub(r'\\textscript +("[^"]*")', r'\\textscript #\1', s)
    return s

# TODO: add lots of these


@rule((1, 3, 98), 'CONTEXT.textStyle -> GROB.#font-style')
def conv(s):
    s = re.sub('\\\\property *"?Voice"? *[.] *"?textStyle"? *= *"([^"]*)"',
                 '\\\\property Voice.TextScript \\\\set #\'font-style = #\'\\1', s)
    s = re.sub('\\\\property *"?Lyrics"? *[.] *"?textStyle"? *= *"([^"]*)"',
                 '\\\\property Lyrics.LyricText \\\\set #\'font-style = #\'\\1', s)

    s = re.sub('\\\\property *"?([^.]+)"? *[.] *"?timeSignatureStyle"? *= *"([^"]*)"',
                 '\\\\property \\1.TimeSignature \\\\override #\'style = #\'\\2', s)

    s = re.sub('"?timeSignatureStyle"? *= *#?""',
                 'TimeSignature \\\\override #\'style = ##f', s)

    s = re.sub('"?timeSignatureStyle"? *= *#?"([^"]*)"',
                 'TimeSignature \\\\override #\'style = #\'\\1', s)

    s = re.sub('#\'style *= #*"([^"])"', '#\'style = #\'\\1', s)

    s = re.sub('\\\\property *"?([^.]+)"? *[.] *"?horizontalNoteShift"? *= *"?#?([-0-9]+)"?',
                 '\\\\property \\1.NoteColumn \\\\override #\'horizontal-shift = #\\2', s)

    # ugh
    s = re.sub('\\\\property *"?([^.]+)"? *[.] *"?flagStyle"? *= *""',
                 '\\\\property \\1.Stem \\\\override #\'flag-style = ##f', s)

    s = re.sub('\\\\property *"?([^.]+)"? *[.] *"?flagStyle"? *= *"([^"]*)"',
                 '\\\\property \\1.Stem \\\\override #\'flag-style = #\'\\2', s)
    return s


@rule((1, 3, 102), r'beamAutoEnd -> autoBeamSettings \push (end * * * *)')
def conv(s):
    s = re.sub('"?beamAutoEnd_([0-9]*)"? *= *(#\\([^)]*\\))',
                 'autoBeamSettings \\\\push #\'(end 1 \\1 * *) = \\2', s)
    s = re.sub('"?beamAutoBegin_([0-9]*)"? *= *(#\\([^)]*\\))',
                 'autoBeamSettings \\\\push #\'(begin 1 \\1 * *) = \\2', s)
    s = re.sub('"?beamAutoEnd"? *= *(#\\([^)]*\\))',
                 'autoBeamSettings \\\\push #\'(end * * * *) = \\1', s)
    s = re.sub('"?beamAutoBegin"? *= *(#\\([^)]*\\))',
                 'autoBeamSettings \\\\push #\'(begin * * * *) = \\1', s)
    return s


@rule((1, 3, 111), r'\push -> \override, \pop -> \revert')
def conv(s):
    s = re.sub('\\\\push', '\\\\override', s)
    s = re.sub('\\\\pop', '\\\\revert', s)
    return s


@rule((1, 3, 113), 'LyricVoice -> LyricsVoice')
def conv(s):
    s = re.sub('LyricVoice', 'LyricsVoice', s)
    # old fix
    s = re.sub('Chord[Nn]ames*.Chord[Nn]ames*', 'ChordNames.ChordName', s)
    s = re.sub('Chord[Nn]ames([ \t\n]+\\\\override)', 'ChordName\\1', s)
    return s


def regularize_id(s):
    s = ''
    lastx = ''
    for x in s:
        if x == '_':
            lastx = x
            continue
        elif x in string.digits:
            x = chr(ord(x) - ord('0') + ord('A'))
        elif x not in string.letters:
            x = 'x'
        elif x in string.lowercase and lastx == '_':
            x = x.upper()
        s = s + x
        lastx = x
    return s


@rule((1, 3, 117), 'identifier names: $!foo_bar_123 -> xfooBarABC')
def conv(s):
    def regularize_dollar_reference(match):
        return regularize_id(match.group(1))

    def regularize_assignment(match):
        return '\n' + regularize_id(match.group(1)) + ' = '
    s = re.sub(r'\$([^\t\n ]+)', regularize_dollar_reference, s)
    s = re.sub('\n([^ \t\n]+)[ \t]*= *', regularize_assignment, s)
    return s


@rule((1, 3, 120), 'paper_xxx -> paperXxxx, pedalup -> pedalUp')
def conv(s):
    def regularize_paper(match):
        return regularize_id(match.group(1))
    s = re.sub('(paper_[a-z]+)', regularize_paper, s)
    s = re.sub('sustainup', 'sustainUp', s)
    s = re.sub('nobreak', 'noBreak', s)
    s = re.sub('sustaindown', 'sustainDown', s)
    s = re.sub('sostenutoup', 'sostenutoUp', s)
    s = re.sub('sostenutodown', 'sostenutoDown', s)
    s = re.sub('unachorda', 'unaChorda', s)
    s = re.sub('trechorde', 'treChorde', s)
    return s


@rule((1, 3, 122), r'drarnChords -> chordChanges, \musicalpitch -> \pitch')
def conv(s):
    s = re.sub('drarnChords', 'chordChanges', s)
    s = re.sub(r'\\musicalpitch', r'\\pitch', s)
    return s


@rule((1, 3, 136), 'ly-X-elt-property -> ly-X-grob-property')
def conv(s):
    s = re.sub('ly-([sg])et-elt-property', 'ly-\\1et-grob-property', s)
    return s


@rule((1, 3, 138), 'point-and-click argument changed to procedure')
def conv(s):
    s = re.sub('point-and-click +#t',
                 'point-and-click line-column-location', s)
    return s


@rule((1, 3, 138), 'followThread -> followVoice')
def conv(s):
    s = re.sub('followThread', 'followVoice', s)
    s = re.sub('Thread.FollowThread', 'Voice.VoiceFollower', s)
    s = re.sub('FollowThread', 'VoiceFollower', s)
    return s


@rule((1, 3, 139), 'font-point-size -> font-design-size')
def conv(s):
    s = re.sub('font-point-size', 'font-design-size', s)
    return s


@rule((1, 3, 141), 'xNoDots -> xSolid')
def conv(s):
    s = re.sub('([a-zA-Z]*)NoDots', '\\1Solid', s)
    return s


@rule((1, 3, 144), 'Chorda -> Corda')
def conv(s):
    s = re.sub('([Cc])hord([ea])', '\\1ord\\2', s)
    return s


@rule((1, 3, 145), 'ContextNameXxxxVerticalExtent -> XxxxVerticalExtent')
def conv(s):
    s = re.sub('([A-Za-z]+)MinimumVerticalExtent',
                 'MinimumV@rticalExtent', s)
    s = re.sub('([A-Za-z]+)ExtraVerticalExtent', 'ExtraV@rticalExtent', s)
    s = re.sub('([A-Za-z]+)VerticalExtent', 'VerticalExtent', s)
    s = re.sub('ExtraV@rticalExtent', 'ExtraVerticalExtent', s)
    s = re.sub('MinimumV@rticalExtent', 'MinimumVerticalExtent', s)
    return s


@rule((1, 3, 146), 'semicolons removed')
def conv(s):
    s = re.sub('\\\\key[ \t]*;', r'\\key \\default;', s)
    s = re.sub('\\\\mark[ \t]*;', r'\\mark \\default;', s)

    # Make sure groups of more than one ; have space before
    # them, so that non of them gets removed by next rule
    s = re.sub("([^ \n\t;]);(;+)", "\\1 ;\\2", s)

    # Only remove ; that are not after spaces, # or ;
    # Otherwise  we interfere with Scheme comments,
    # which is badbadbad.
    s = re.sub("([^ \t;#]);", "\\1", s)
    return s


@rule((1, 3, 147), 'default-neutral-direction -> neutral-direction')
def conv(s):
    s = re.sub('default-neutral-direction', 'neutral-direction', s)
    return s


@rule((1, 3, 148), '"(align" -> "(axis", "(rows" -> "(columns"')
def conv(s):
    s = re.sub(r'\(align', '(axis', s)
    s = re.sub(r'\(rows', '(columns', s)
    return s


@rule((1, 5, 33), 'SystemStartDelimiter -> systemStartDelimiter')
def conv(s):
    s = re.sub('SystemStartDelimiter', 'systemStartDelimiter', s)
    return s


@rule((1, 5, 38), 'arithmetic... -> spacing...')
def conv(s):
    s = re.sub('arithmetic-multiplier', 'spacing-increment', s)
    s = re.sub('arithmetic-basicspace', 'shortest-duration-space', s)
    return s


# 40 ?
@rule((1, 5, 40), 'breakAlignOrder property names')
def conv(s):

    def func(match):
        break_dict = {
            "Instrument_name": "instrument-name",
            "Left_edge_item": "left-edge",
            "Span_bar": "span-bar",
            "Breathing_sign": "breathing-sign",
            "Staff_bar": "staff-bar",
            "Clef_item": "clef",
            "Key_item": "key-signature",
            "Time_signature": "time-signature",
            "Custos": "custos"
        }
        props = match.group(1)
        for (k, v) in list(break_dict.items()):
            props = re.sub(k, v, props)
        return "breakAlignOrder = #'(%s)" % props

    s = re.sub("breakAlignOrder *= *#'\\(([a-z_\n\tA-Z ]+)\\)",
                 func, s)
    return s


@rule((1, 5, 49), 'noAutoBeaming -> autoBeaming')
def conv(s):
    s = re.sub('noAutoBeaming *= *##f', 'autoBeaming = ##t', s)
    s = re.sub('noAutoBeaming *= *##t', 'autoBeaming = ##f', s)
    return s


@rule((1, 5, 52), 'tuplet-X-visibility -> X-visibility')
def conv(s):
    s = re.sub('tuplet-bracket-visibility', 'bracket-visibility', s)
    s = re.sub('tuplet-number-visibility', 'number-visibility', s)
    return s


@rule((1, 5, 56), 'Pitch::transpose -> ly-transpose-pitch')
def conv(s):
    s = re.sub('Pitch::transpose', 'ly-transpose-pitch', s)
    return s


@rule((1, 5, 58), 'deprecate textNonEmpty')
def conv(s):
    s = re.sub('textNonEmpty *= *##t',
                 "TextScript \\\\set #'no-spacing-rods = ##f", s)
    s = re.sub('textNonEmpty *= *##f',
                 "TextScript \\\\set #'no-spacing-rods = ##t", s)
    return s


@rule((1, 5, 59), 'XxxxVerticalExtent -> xxxVerticalExtent')
def conv(s):
    s = re.sub('MinimumVerticalExtent', 'minimumV@rticalExtent', s)
    s = re.sub('minimumVerticalExtent', 'minimumV@rticalExtent', s)
    s = re.sub('ExtraVerticalExtent', 'extraV@rticalExtent', s)
    s = re.sub('extraVerticalExtent', 'extraV@rticalExtent', s)
    s = re.sub('VerticalExtent', 'verticalExtent', s)
    s = re.sub('extraV@rticalExtent', 'extraVerticalExtent', s)
    s = re.sub('minimumV@rticalExtent', 'minimumVerticalExtent', s)
    return s


@rule((1, 5, 62), 'visibility-lambda -> break-visibility')
def conv(s):
    s = re.sub('visibility-lambda', 'break-visibility', s)
    return s


@rule((1, 5, 67), 'automaticMelismata turned on by default')
def conv(s):
    if re.search(r'\\addlyrics', s) \
            and re.search('automaticMelismata', s) is None:
        stderr_write(NOT_SMART % "automaticMelismata")
        stderr_write(
            _("automaticMelismata is turned on by default since 1.5.67."))
        stderr_write('\n')
        raise FatalConversionError()
    return s


@rule((1, 5, 68), 'ly-set-X-property -> ly-set-X-property!')
def conv(s):
    s = re.sub('ly-set-grob-property([^!])', 'ly-set-grob-property!\1', s)
    s = re.sub('ly-set-mus-property([^!])', 'ly-set-mus-property!\1', s)
    return s


@rule((1, 5, 71), 'extent-[XY] -> [XY]-extent')
def conv(s):
    s = re.sub('extent-X', 'X-extent', s)
    s = re.sub('extent-Y', 'Y-extent', s)
    return s


@rule((1, 5, 72), 'set! point-and-click -> set-point-and-click!')
def conv(s):
    s = re.sub(r"#\(set! +point-and-click +line-column-location\)",
                 "#(set-point-and-click! 'line-column)", s)
    s = re.sub(r"#\(set![ \t]+point-and-click +line-location\)",
                 "#(set-point-and-click! 'line)", s)
    s = re.sub(r'#\(set! +point-and-click +#f\)',
                 "#(set-point-and-click! 'none)", s)
    return s


@rule((1, 6, 5), 'Stems: flag-style -> stroke-style; style -> flag-style')
def conv(s):
    s = re.sub('flag-style', 'stroke-style', s)
    s = re.sub(r"""Stem([ ]+)\\override #'style""",
                 r"""Stem \\override #'flag-style""", s)
    s = re.sub(r"""Stem([ ]+)\\set([ ]+)#'style""",
                 r"""Stem \\set #'flag-style""", s)
    return s


def subst_req_name(match):
    return "(make-music-by-name \'%sEvent)" % regularize_id(match.group(1))


@rule((1, 7, 1), 'ly-make-music foo_bar_req -> make-music-by-name FooBarEvent')
def conv(s):
    s = re.sub(
        '\\(ly-make-music *"([A-Z][a-z_]+)_req"\\)', subst_req_name, s)
    s = re.sub('Request_chord', 'EventChord', s)
    return s


spanner_subst = {
    "text": 'TextSpanEvent',
    "decrescendo": 'DecrescendoEvent',
    "crescendo": 'CrescendoEvent',
    "Sustain": 'SustainPedalEvent',
    "slur": 'SlurEvent',
    "UnaCorda": 'UnaCordaEvent',
    "Sostenuto": 'SostenutoEvent',
}


def subst_ev_name(match):
    stype = 'STOP'
    if re.search('start', match.group(1)):
        stype = 'START'
    mtype = spanner_subst[match.group(2)]
    return "(make-span-event '%s %s)" % (mtype, stype)


def subst_definition_ev_name(match):
    return ' = #%s' % subst_ev_name(match)


def subst_inline_ev_name(match):
    s = subst_ev_name(match)
    return '#(ly-export %s)' % s


def subst_csp_definition(match):
    return ' = #(make-event-chord (list %s))' % subst_ev_name(match)


def subst_csp_inline(match):
    return '#(ly-export (make-event-chord (list %s)))' % subst_ev_name(match)


@rule((1, 7, 2), r'''\spanrequest -> #(make-span-event ... ),
\script -> #(make-articulation ... )''')
def conv(s):
    s = re.sub(
        r' *= *\\spanrequest *([^ ]+) *"([^"]+)"', subst_definition_ev_name, s)
    s = re.sub(
        r'\\spanrequest *([^ ]+) *"([^"]+)"', subst_inline_ev_name, s)
    s = re.sub(
        r' *= *\\commandspanrequest *([^ ]+) *"([^"]+)"', subst_csp_definition, s)
    s = re.sub(
        r'\\commandspanrequest *([^ ]+) *"([^"]+)"', subst_csp_inline, s)
    s = re.sub(r'ly-id ', 'ly-import ', s)

    s = re.sub(r' *= *\\script "([^"]+)"',
                 ' = #(make-articulation "\\1")', s)
    s = re.sub(r'\\script "([^"]+)"',
                 '#(ly-export (make-articulation "\\1"))', s)
    return s


@rule((1, 7, 3), 'ly- -> ly:')
def conv(s):
    s = re.sub(r'\(ly-', '(ly:', s)

    changed = [
        r'duration\?',
        r'font-metric\?',
        r'molecule\?',
        r'moment\?',
        r'music\?',
        r'pitch\?',
        'make-duration',
        'music-duration-length',
        'duration-log',
        'duration-dotcount',
        'intlog2',
        'duration-factor',
        'transpose-key-alist',
        'get-system',
        'get-broken-into',
        'get-original',
        'set-point-and-click!',
        'make-moment',
        'make-pitch',
        'pitch-octave',
        'pitch-alteration',
        'pitch-notename',
        'pitch-semitones',
        r'pitch<\?',
        r'dir\?',
        'music-duration-compress',
        'set-point-and-click!'
    ]

    origre = r'\b(%s)' % '|'.join(changed)

    s = re.sub(origre, r'ly:\1', s)
    s = re.sub('set-point-and-click!', 'set-point-and-click', s)
    return s


@rule((1, 7, 4), '<< >> -> < <  > >')
def conv(s):
    if re.search('new-chords-done', s):
        return s

    s = re.sub(r'<<', '< <', s)
    s = re.sub(r'>>', '> >', s)
    return s


@rule((1, 7, 5), r'\transpose TO -> \transpose FROM  TO')
def conv(s):
    s = re.sub(r"\\transpose", r"\\transpose c'", s)
    s = re.sub(r"\\transpose c' *([a-z]+)'", r"\\transpose c \1", s)
    return s


@rule((1, 7, 6), r'note\script -> note-\script')
def conv(s):
    kws = ['arpeggio',
           'sustainDown',
           'sustainUp',
           'f',
           'p',
           'pp',
           'ppp',
           'fp',
           'ff',
           'mf',
           'mp',
           'sfz',
           ]

    origstr = '|'.join(kws)
    s = re.sub(r'([^_^-])\\(%s)\b' % origstr, r'\1-\\\2', s)
    return s


@rule((1, 7, 10), r"\property ChordName #'style -> #(set-chord-name-style 'style)")
def conv(s):
    s = re.sub(r"\\property *ChordNames *\. *ChordName *\\(set|override) *#'style *= *#('[a-z]+)",
                 r"#(set-chord-name-style \2)", s)
    s = re.sub(r"\\property *ChordNames *\. *ChordName *\\revert *#'style",
                 r"", s)
    return s


@rule((1, 7, 11), "transpose-pitch -> pitch-transpose")
def conv(s):
    s = re.sub(r"ly:transpose-pitch", "ly:pitch-transpose", s)
    return s


@rule((1, 7, 13), "ly:XX-molecule-YY -> ly:molecule-XX-YY")
def conv(s):
    s = re.sub(r"ly:get-molecule-extent", "ly:molecule-get-extent", s)
    s = re.sub(r"ly:set-molecule-extent!", "ly:molecule-set-extent!", s)
    s = re.sub(r"ly:add-molecule", "ly:molecule-add", s)
    s = re.sub(r"ly:combine-molecule-at-edge",
                 "ly:molecule-combine-at-edge", s)
    s = re.sub(r"ly:align-to!", "ly:molecule-align-to!", s)
    return s


@rule((1, 7, 15), "linewidth = -1 -> raggedright = ##t")
def conv(s):
    s = re.sub(
        r"linewidth *= *-[0-9.]+ *(\\mm|\\cm|\\in|\\pt)?", 'raggedright = ##t', s)
    return s


@rule((1, 7, 16), "divisiomaior -> divisioMaior")
def conv(s):
    s = re.sub("divisiomaior",
                 "divisioMaior", s)
    s = re.sub("divisiominima",
                 "divisioMinima", s)
    s = re.sub("divisiomaxima",
                 "divisioMaxima", s)
    return s


@rule((1, 7, 17), "Skip_req  -> Skip_event")
def conv(s):
    s = re.sub("Skip_req_swallow_translator",
                 "Skip_event_swallow_translator", s)
    return s


@rule((1, 7, 18), "groupOpen/Close  -> start/stopGroup, #'outer  -> #'enclose-bounds")
def conv(s):
    s = re.sub("groupOpen",
                 "startGroup", s)
    s = re.sub("groupClose",
                 "stopGroup", s)
    s = re.sub("#'outer",
                 "#'enclose-bounds", s)

    return s


@rule((1, 7, 19), "remove GraceContext")
def conv(s):
    if re.search(r'\\GraceContext', s):
        stderr_write(NOT_SMART % "GraceContext")
        stderr_write(FROM_TO
                     % ("GraceContext", "#(add-to-grace-init .. )"))
        stderr_write(UPDATE_MANUALLY)
        raise FatalConversionError()

    s = re.sub('HaraKiriStaffContext', 'RemoveEmptyStaffContext', s)
    return s


@rule((1, 7, 22), "#'type -> #'style")
def conv(s):
    s = re.sub(
        r"(set|override|revert) *#'type",
        r"\1 #'style",
        s)
    return s


@rule((1, 7, 23), "barNonAuto -> automaticBars")
def conv(s):
    s = re.sub(
        "barNonAuto *= *##t",
        "automaticBars = ##f",
        s)
    s = re.sub(
        "barNonAuto *= *##f",
        "automaticBars = ##t",
        s)
    return s


@rule((1, 7, 24), "cluster syntax")
def conv(s):
    if re.search(r'-(start|stop)Cluster', s):
        stderr_write(NOT_SMART % _("cluster syntax"))
        stderr_write(UPDATE_MANUALLY)

        raise FatalConversionError()
    return s


@rule((1, 7, 28), "new Pedal style syntax")
def conv(s):
    s = re.sub(r"\\property *Staff\.(Sustain|Sostenuto|UnaCorda)Pedal *\\(override|set) *#'pedal-type *",
                 r"\\property Staff.pedal\1Style ", s)
    s = re.sub(
        r"\\property *Staff\.(Sustain|Sostenuto|UnaCorda)Pedal *\\revert *#'pedal-type", '', s)
    return s


def sub_chord(m):
    s = m.group(1)

    origstr = '<%s>' % s
    if re.search(r'\\\\', s):
        return origstr

    if re.search(r'\\property', s):
        return origstr

    if re.match(r'^\s*\)?\s*\\[a-zA-Z]+', s):
        return origstr

    durs = []

    def sub_durs(m, durs=durs):
        durs.append(m.group(2))
        return m.group(1)

    s = re.sub(r"([a-z]+[,'!? ]*)([0-9]+\.*)", sub_durs, s)
    dur_str = ''

    for d in durs:
        if dur_str == '':
            dur_str = d
        if dur_str != d:
            return '<%s>' % m.group(1)

    pslur_strs = ['']
    dyns = ['']
    slur_strs = ['']

    last_str = ''
    while last_str != s:
        last_str = s

        def sub_tremolos(m, slur_strs=slur_strs):
            tr = m.group(2)
            if tr not in slur_strs:
                slur_strs.append(tr)
            return m.group(1)

        s = re.sub(r"([a-z]+[',!? ]*)(:[0-9]+)",
                     sub_tremolos, s)

        def sub_dyn_end(m, dyns=dyns):
            dyns.append(r' \!')
            return ' ' + m.group(2)

        s = re.sub(r'(\\!)\s*([a-z]+)', sub_dyn_end, s)

        def sub_slurs(m, slur_strs=slur_strs):
            if '-)' not in slur_strs:
                slur_strs.append(')')
            return m.group(1)

        def sub_p_slurs(m, slur_strs=slur_strs):
            if r'-\)' not in slur_strs:
                slur_strs.append(r'\)')
            return m.group(1)

        s = re.sub(r"\)[ ]*([a-z]+)", sub_slurs, s)
        s = re.sub(r"\\\)[ ]*([a-z]+)", sub_p_slurs, s)

        def sub_begin_slurs(m, slur_strs=slur_strs):
            if '-(' not in slur_strs:
                slur_strs.append('(')
            return m.group(1)

        s = re.sub(r"([a-z]+[,'!?0-9 ]*)\(",
                     sub_begin_slurs, s)

        def sub_begin_p_slurs(m, slur_strs=slur_strs):
            if r'-\(' not in slur_strs:
                slur_strs.append(r'\(')
            return m.group(1)

        s = re.sub(r"([a-z]+[,'!?0-9 ]*)\\\(",
                     sub_begin_p_slurs, s)

        def sub_dyns(m, slur_strs=slur_strs):
            s = m.group(0)
            if s == '@STARTCRESC@':
                slur_strs.append("\\<")
            elif s == '@STARTDECRESC@':
                slur_strs.append("\\>")
            elif s == r'-?\\!':
                slur_strs.append('\\!')
            return ''

        s = re.sub(r'@STARTCRESC@', sub_dyns, s)
        s = re.sub(r'-?\\!', sub_dyns, s)

        def sub_articulations(m, slur_strs=slur_strs):
            a = m.group(1)
            if a not in slur_strs:
                slur_strs.append(a)
            return ''

        s = re.sub(r"([_^-]\@ACCENT\@)", sub_articulations,
                     s)
        s = re.sub(r"([_^-]\\[a-z]+)", sub_articulations,
                     s)
        s = re.sub(r"([_^-][>_.+|^-])", sub_articulations,
                     s)
        s = re.sub(r'([_^-]"[^"]+")', sub_articulations,
                     s)

        def sub_pslurs(m, slur_strs=slur_strs):
            slur_strs.append(' \\)')
            return m.group(1)
        s = re.sub(r"\\\)[ ]*([a-z]+)", sub_pslurs, s)

    # end of while <>

    suffix = ''.join(slur_strs) + ''.join(pslur_strs) \
             + ''.join(dyns)

    return '@STARTCHORD@%s@ENDCHORD@%s%s' % (s, dur_str, suffix)


def sub_chords(s):
    simend = '>'
    simstart = '<'
    chordstart = '<<'
    chordend = '>>'
    marker_str = '%% new-chords-done %%'

    if re.search(marker_str, s):
        return s
    s = re.sub('<<', '@STARTCHORD@', s)
    s = re.sub('>>', '@ENDCHORD@', s)

    s = re.sub(r'\\<', '@STARTCRESC@', s)
    s = re.sub(r'\\>', '@STARTDECRESC@', s)
    s = re.sub(r'([_^-])>', r'\1@ACCENT@', s)
    s = re.sub(r'<([^<>{}]+)>', sub_chord, s)

    # add dash: -[, so that [<<a b>> c d] becomes
    #                      <<a b>>-[ c d]
    # and gets skipped by articulation_substitute
    s = re.sub(r'\[ *(@STARTCHORD@[^@]+@ENDCHORD@[0-9.]*)',
                 r'\1-[', s)
    s = re.sub(r'\\! *(@STARTCHORD@[^@]+@ENDCHORD@[0-9.]*)',
                 r'\1-\\!', s)

    s = re.sub(r'<([^?])', r'%s\1' % simstart, s)
    s = re.sub(r'>([^?])', r'%s\1' % simend,  s)
    s = re.sub('@STARTCRESC@', r'\\<', s)
    s = re.sub('@STARTDECRESC@', r'\\>', s)
    s = re.sub(r'\\context *Voice *@STARTCHORD@',
                 '@STARTCHORD@', s)
    s = re.sub('@STARTCHORD@', chordstart, s)
    s = re.sub('@ENDCHORD@', chordend, s)
    s = re.sub(r'@ACCENT@', '>', s)
    return s


markup_start = re.compile(r"([-^_]|\\mark)\s*(#\s*'\s*)\(")
musicglyph = re.compile(r"\(\s*music\b")
columns = re.compile(r"\(\s*columns\b")
submarkup_start = re.compile(r"\(\s*([a-zA-Z]+)")
leftpar = re.compile(r"\(")
rightpar = re.compile(r"\)")


def text_markup(s):
    result = ''
    # Find the beginning of each markup:
    match = markup_start.search(s)
    while match:
        result = result + s[:match.end(1)] + r" \markup"
        s = s[match.end(2):]
        # Count matching parentheses to find the end of the
        # current markup:
        nesting_level = 0
        pars = re.finditer(r"[()]", s)
        for par in pars:
            if par.group() == '(':
                nesting_level = nesting_level + 1
            else:
                nesting_level = nesting_level - 1
            if nesting_level == 0:
                markup_end = par.end()
                break
        # The full markup in old syntax:
        markup = s[:markup_end]
        # Modify to new syntax:
        markup = musicglyph.sub(r"{\\musicglyph", markup)
        markup = columns.sub(r"{", markup)
        markup = submarkup_start.sub(r"{\\\1", markup)
        markup = leftpar.sub("{", markup)
        markup = rightpar.sub("}", markup)

        result = result + markup
        # Find next markup
        s = s[markup_end:]
        match = markup_start.search(s)
    result = result + s
    return result


def articulation_substitute(s):
    s = re.sub(r"""([^-])\[ *(\\?\)?[a-z]+[,']*[!?]?[0-9:]*\.*)""",
                 r"\1 \2[", s)
    s = re.sub(r"""([^-])\\\) *([a-z]+[,']*[!?]?[0-9:]*\.*)""",
                 r"\1 \2\\)", s)
    s = re.sub(r"""([^-\\])\) *([a-z]+[,']*[!?]?[0-9:]*\.*)""",
                 r"\1 \2)", s)
    s = re.sub(r"""([^-])\\! *([a-z]+[,']*[!?]?[0-9:]*\.*)""",
                 r"\1 \2\\!", s)
    return s


string_or_scheme = re.compile('("(?:[^"\\\\]|\\\\.)*")|(#\\s*\'?\\s*\\()')

# Only apply articulation_substitute () outside strings and
# Scheme expressions:


def smarter_articulation_subst(s):
    result = ''
    # Find the beginning of next string or Scheme expr.:
    match = string_or_scheme.search(s)
    while match:
        # Convert the preceding LilyPond code:
        previous_chunk = s[:match.start()]
        result = result + articulation_substitute(previous_chunk)
        if match.group(1):  # Found a string
            # Copy the string to output:
            result = result + match.group(1)
            s = s[match.end(1):]
        else:  # Found a Scheme expression. Count
            # matching parentheses to find its end
            s = s[match.start():]
            nesting_level = 0
            pars = re.finditer(r"[()]", s)
            for par in pars:
                if par.group() == '(':
                    nesting_level = nesting_level + 1
                else:
                    nesting_level = nesting_level - 1
                if nesting_level == 0:
                    scheme_end = par.end()
                    break
            # Copy the Scheme expression to output:
            result = result + s[:scheme_end]
            s = s[scheme_end:]
        # Find next string or Scheme expression:
        match = string_or_scheme.search(s)
    # Convert the remainder of the file
    result = result + articulation_substitute(s)
    return result


def conv_relative(s):
    if re.search(r"\\relative", s):
        s = "#(ly:set-option 'old-relative)\n" + s

    return s


@rule((1, 9, 0), """New relative mode,
Postfix articulations, new text markup syntax, new chord syntax""")
def conv(s):
    s = re.sub(r"#'\(\)", "@SCM_EOL@", s)
    s = conv_relative(s)
    s = sub_chords(s)

    s = text_markup(s)
    s = smarter_articulation_subst(s)
    s = re.sub("@SCM_EOL@", "#'()", s)
    return s


@rule((1, 9, 1), "Remove - before articulation")
def conv(s):
    if re.search("font-style", s):
        stderr_write(NOT_SMART % "font-style")
        stderr_write(UPDATE_MANUALLY)

        raise FatalConversionError()

    s = re.sub(r'-\\markup', r'@\\markup', s)
    s = re.sub(r'-\\', r'\\', s)
    s = re.sub(r'-\)', ')', s)
    s = re.sub(r'-\(', '(', s)
    s = re.sub(r'-\[', '[', s)
    s = re.sub(r'-\]', ']', s)
    s = re.sub('-~', '~', s)
    s = re.sub(r'@\\markup', r'-\\markup', s)
    return s


@rule((1, 9, 2), r"\newcontext -> \new")
def conv(s):
    s = re.sub('ly:set-context-property',
                 'ly:set-context-property!', s)
    s = re.sub('\\\\newcontext', '\\\\new', s)
    s = re.sub('\\\\grace[\t\n ]*([^{ ]+)',
                 r'\\grace { \1 }', s)
    s = re.sub("\\\\grace[\t\n ]*{([^}]+)}",
                 r"""\\grace {
\\property Voice.Stem \\override #'stroke-style = #"grace"
  \1
  \\property Voice.Stem \\revert #'stroke-style }
""", s)
    return s


@rule((1, 9, 3), r"""\acciaccatura misspelling,
fingerHorizontalDirection -> fingeringOrientations""")
def conv(s):
    s = re.sub('accacciatura',
                 'acciaccatura', s)

    if re.search("context-spec-music", s):
        stderr_write(NOT_SMART % "context-spec-music")
        stderr_write(UPDATE_MANUALLY)

        raise FatalConversionError()

    s = re.sub('fingerHorizontalDirection *= *#(LEFT|-1)',
                 "fingeringOrientations = #'(up down left)", s)
    s = re.sub('fingerHorizontalDirection *= *#(RIGHT|1)',
                 "fingeringOrientations = #'(up down right)", s)
    return s


@rule((1, 9, 4), 'Swap < > and << >>')
def conv(s):
    if re.search('\\figures', s):
        warning(_("attempting automatic \\figures conversion.  Check results!"))

    def figures_replace(m):
        s = m.group(1)
        s = re.sub('<', '@FIGOPEN@', s)
        s = re.sub('>', '@FIGCLOSE@', s)
        return '\\figures { %s }' % s

    s = re.sub(r'\\figures[ \t\n]*{([^}]+)}', figures_replace, s)
    s = re.sub(r'\\<', '@STARTCRESC@', s)
    s = re.sub(r'\\>', '@STARTDECRESC@', s)
    s = re.sub(r'([-^_])>', r'\1@ACCENT@', s)
    s = re.sub(r'<<', '@STARTCHORD@', s)
    s = re.sub(r'>>', '@ENDCHORD@', s)
    s = re.sub(r'>', '@ENDSIMUL@', s)
    s = re.sub(r'<', '@STARTSIMUL@', s)
    s = re.sub('@STARTDECRESC@', r'\\>', s)
    s = re.sub('@STARTCRESC@', r'\\<', s)
    s = re.sub('@ACCENT@', '>', s)
    s = re.sub('@ENDCHORD@', '>', s)
    s = re.sub('@STARTCHORD@', '<', s)
    s = re.sub('@STARTSIMUL@', '<<', s)
    s = re.sub('@ENDSIMUL@', '>>', s)
    s = re.sub('@FIGOPEN@', '<', s)
    s = re.sub('@FIGCLOSE@', '>', s)
    return s


@rule((1, 9, 5), 'HaraKiriVerticalGroup -> RemoveEmptyVerticalGroup')
def conv(s):
    s = re.sub('HaraKiriVerticalGroup', 'RemoveEmptyVerticalGroup', s)
    return s


@rule((1, 9, 6), 'deprecate ly:get-font')
def conv(s):
    if re.search("ly:get-font", s):
        stderr_write(NOT_SMART % "ly:get-font")
        stderr_write(FROM_TO
                     % ("(ly:paper-get-font (ly:grob-get-paper foo) .. )",
                         "(ly:paper-get-font (ly:grob-get-paper foo) .. )"))
        stderr_write(UPDATE_MANUALLY)
        raise FatalConversionError()

    if re.search(r"\\pitch *#", s):
        stderr_write(NOT_SMART % "\\pitch")
        stderr_write(_("Use Scheme code to construct arbitrary note events."))
        stderr_write('\n')

        raise FatalConversionError()
    return s


@rule((1, 9, 7), r'''use symbolic constants for alterations,
remove \outputproperty, move ly:verbose into ly:get-option''')
def conv(s):
    def sub_alteration(m):
        alt = m.group(3)
        alt = {
            '-1': 'FLAT',
            '-2': 'DOUBLE-FLAT',
            '0': 'NATURAL',
            '1': 'SHARP',
            '2': 'DOUBLE-SHARP',
        }[alt]

        return '(ly:make-pitch %s %s %s)' % (m.group(1), m.group(2),
                                             alt)

    s = re.sub("\\(ly:make-pitch *([0-9-]+) *([0-9-]+) *([0-9-]+) *\\)",
                 sub_alteration, s)

    s = re.sub("ly:verbose", "ly:get-option 'verbose", s)

    m = re.search("\\\\outputproperty #([^#]+)[\t\n ]*#'([^ ]+)", s)
    if m:
        stderr_write(_(
            r"""\outputproperty found,
Please hand-edit, using

  \applyoutput #(outputproperty-compatibility %s '%s <GROB PROPERTY VALUE>)

as a substitution text""") % (m.group(1), m.group(2)))
        raise FatalConversionError()

    if re.search("ly:(make-pitch|pitch-alteration)", s) \
            or re.search("keySignature", s):
        stderr_write(NOT_SMART % "pitches")
        stderr_write(
            _("""The alteration field of Scheme pitches was multiplied by 2
to support quarter tone accidentals.  You must update the following constructs manually:

* calls of ly:make-pitch and ly:pitch-alteration
* keySignature settings made with \\property
"""))
        raise FatalConversionError()
    return s


@rule((1, 9, 8), "dash-length -> dash-fraction")
def conv(s):
    if re.search("dash-length", s):
        stderr_write(NOT_SMART % "dash-length")
        stderr_write(FROM_TO % ("dash-length", "dash-fraction"))
        stderr_write(UPDATE_MANUALLY)
        raise FatalConversionError()
    return s


@rule((2, 1, 1), "font-relative-size -> font-size")
def conv(s):
    def func(match):
        return "#'font-size = #%d" % (2*int(match.group(1)))

    s = re.sub(r"#'font-relative-size\s*=\s*#\+?([0-9-]+)", func, s)
    s = re.sub(r"#'font-family\s*=\s*#'ancient",
                 r"#'font-family = #'music", s)
    return s


@rule((2, 1, 2), "ly:get-music-length -> ly:music-length")
def conv(s):
    s = re.sub(r"ly:get-music-length", "ly:music-length", s)
    return s


@rule((2, 1, 3), "stanza -> instrument")
def conv(s):
    s = re.sub(r"\.\s+stz=", ". instr ", s)
    return s


@rule((2, 1, 4), "removal of automaticMelismata; use melismaBusyProperties instead")
def conv(s):
    def func(match):
        c = match.group(1)
        b = match.group(2)

        if b == 't':
            if c == 'Score':
                return ''
            return r" \property %s.melismaBusyProperties \unset" % c

        assert b == 'f', "Value must be ##t or ##f and not ##%s" % b
        return r"\property %s.melismaBusyProperties = #'(melismaBusy)" % c

    s = re.sub(
        r"\\property ([a-zA-Z]+)\s*\.\s*automaticMelismata\s*=\s*##([ft])", func, s)
    return s


@rule((2, 1, 7), r"\translator Staff -> \change Staff")
def conv(s):
    s = re.sub(r"\\translator\s+([a-zA-Z]+)", r"\\change \1", s)
    return s


@rule((2, 1, 10), r"\newaddlyrics -> \lyricsto")
def conv(s):
    s = re.sub(r"\\newaddlyrics", r"\\lyricsto", s)
    return s


@rule((2, 1, 11), r"""\include "paper16.ly" -> #(set-staff-size 16)
\note #3 #1 #1 -> \note #"8." #1
""")
def conv(s):
    s = re.sub(r'\\include\s*"paper([0-9]+)(-init)?.ly"',
                 r"#(set-staff-size \1)", s)

    def sub_note(match):
        dur = ''
        log = int(match.group(1))
        dots = int(match.group(2))

        if log >= 0:
            dur = '%d' % (1 << log)
        else:
            dur = {-1: 'breve',
                   -2: 'longa',
                   -3: 'maxima'}[log]

        dur += ('.' * dots)

        return r'\note #"%s" #%s' % (dur, match.group(3))

    s = re.sub(r'\\note\s+#([0-9-]+)\s+#([0-9]+)\s+#([0-9.-]+)',
                 sub_note, s)
    return s


@rule((2, 1, 12), "OttavaSpanner -> OttavaBracket")
def conv(s):
    s = re.sub(r"OttavaSpanner", r"OttavaBracket", s)
    return s


@rule((2, 1, 13), "set-staff-size -> set-global-staff-size")
def conv(s):
    s = re.sub(r"\(set-staff-size ", r"(set-global-staff-size ", s)
    return s


@rule((2, 1, 14), "style = dotted -> dash-fraction = 0")
def conv(s):
    s = re.sub(r"#'style\s*=\s*#'dotted-line",
                 r"#'dash-fraction = #0.0 ", s)
    return s


@rule((2, 1, 15), "LyricsVoice . instr(ument) -> vocalName")
def conv(s):
    s = re.sub(r'LyricsVoice\s*\.\s*instrument\s*=\s*("[^"]*")',
                 r'LyricsVoice . vocalName = \1', s)

    s = re.sub(r'LyricsVoice\s*\.\s*instr\s*=\s*("[^"]*")',
                 r'LyricsVoice . vocNam = \1', s)
    return s


@rule((2, 1, 16), r'\musicglyph #"accidentals-NUM" -> \sharp/flat/etc.')
def conv(s):
    def sub_acc(m):
        d = {
            '4': 'doublesharp',
            '3': 'threeqsharp',
            '2': 'sharp',
            '1': 'semisharp',
            '0': 'natural',
            '-1': 'semiflat',
            '-2': 'flat',
            '-3': 'threeqflat',
            '-4': 'doubleflat'}
        return '\\%s' % d[m.group(1)]

    s = re.sub(r'\\musicglyph\s*#"accidentals-([0-9-]+)"',
                 sub_acc, s)
    return s


@rule((2, 1, 17), r"\partcombine syntax change to \newpartcombine")
def conv(s):

    if re.search(r'\\partcombine', s):
        stderr_write(NOT_SMART % "\\partcombine")
        stderr_write(UPDATE_MANUALLY)
        raise FatalConversionError()

    # this rule doesn't really work,
    # too lazy to figure out why.
    s = re.sub(r'\\context\s+Voice\s*=\s*one\s*\\partcombine\s+Voice\s*\\context\s+Thread\s*=\s*one(.*)\s*'
                 + r'\\context\s+Thread\s*=\s*two',
                 '\\\\newpartcombine\n\\1\n', s)
    return s


@rule((2, 1, 18), r"""\newpartcombine -> \partcombine,
\autochange Staff -> \autochange""")
def conv(s):
    s = re.sub(r'\\newpartcombine', r'\\partcombine', s)
    s = re.sub(r'\\autochange\s+Staff', r'\\autochange ', s)
    return s


@rule((2, 1, 19), r"""Drum notation changes, Removing \chordmodifiers, \notenames.
Harmonic notes. Thread context removed. Lyrics context removed""")
def conv(s):
    if re.search('include "drumpitch', s):
        stderr_write(_("Drums found. Enclose drum notes in \\drummode"))

    s = re.sub(r'\\include "drumpitch-init.ly"', '', s)

    s = re.sub(r'\\pitchnames ', 'pitchnames = ', s)
    s = re.sub(r'\\chordmodifiers ', 'chordmodifiers = ', s)
    s = re.sub(r'\bdrums\b\s*=', 'drumContents = ', s)
    s = re.sub(r'\\drums\b', r'\\drumContents ', s)

    if re.search('drums->paper', s):
        stderr_write(_("\n%s found. Check file manually!\n") %
                     _("Drum notation"))

    s = re.sub(r"""\\apply\s+#\(drums->paper\s+'([a-z]+)\)""",
                 r"""\\property DrumStaff.drumStyleTable = #\1-style""",
                 s)

    if re.search('Thread', s):
        stderr_write(_("\n%s found. Check file manually!\n") % "Thread")

    s = re.sub(r"""(\\once\s*)?\\property\s+Thread\s*\.\s*NoteHead\s*"""
                 + r"""\\(set|override)\s*#'style\s*=\s*#'harmonic"""
                 + r"""\s+([a-z]+[,'=]*)([0-9]*\.*)""", r"""<\3\\harmonic>\4""", s)

    s = re.sub(r"""\\new Thread""", r"""\\context Voice""", s)
    s = re.sub(r"""Thread""", """Voice""", s)

    if re.search('\bLyrics\b', s):
        stderr_write(_("\n%s found. Check file manually!\n") % "Lyrics")

    s = re.sub(r"""LyricsVoice""", r"""L@ricsVoice""", s)
    s = re.sub(r"""\bLyrics\b""", r"""LyricsVoice""", s)
    s = re.sub(r"""LyricsContext""", r"""LyricsVoiceContext""", s)
    s = re.sub(r"""L@ricsVoice""", r"""LyricsVoice""", s)
    return s


@rule((2, 1, 20), "nonevent-skip -> skip-music")
def conv(s):
    s = re.sub(r'nonevent-skip', 'skip-music', s)
    return s


@rule((2, 1, 21), r"""molecule-callback -> print-function,
brew_molecule -> print
brew-new-markup-molecule -> Text_item::print
LyricsVoice -> Lyrics
tupletInvisible -> TupletBracket \set #'transparent
remove Grob::preset_extent""")
def conv(s):
    s = re.sub(r'molecule-callback', 'print-function', s)
    s = re.sub(r'brew_molecule', 'print', s)
    s = re.sub(r'brew-new-markup-molecule', 'Text_item::print', s)
    s = re.sub(r'LyricsVoice', 'Lyrics', s)
    s = re.sub(r'tupletInvisible',
                 r"TupletBracket \\set #'transparent", s)
#       s = re.sub (r'molecule', 'collage', s)
# molecule -> collage
    s = re.sub(r"\\property\s+[a-zA-Z]+\s*\.\s*[a-zA-Z]+\s*"
                 + r"\\set\s*#'X-extent-callback\s*=\s*#Grob::preset_extent",
                 "", s)
    return s


@rule((2, 1, 22), r"""new syntax for property settings:
  \set A.B = #C , \unset A.B
  \override A.B #C = #D, \revert A.B #C
""")
def conv(s):
    s = re.sub(r'(\\property[^=]+)=\s*([-0-9]+)',
                 r'\1= #\2', s)
    s = re.sub(r'\\property\s+([^. ]+)\s*\.\s*([^\\=]+)\s*\\(set|override)',
                 r"\\overrid@ \1.\2 ", s)
    s = re.sub(r'\\property\s+([^. ]+)\s*\.\s*([^\\= ]+)\s*=\s*',
                 r'\\s@t \1.\2 = ', s)
    s = re.sub(r'\\property\s+([^. ]+)\s*\.\s*([^\\= ]+)\s*\\unset',
                 r'\\uns@t \1.\2 ', s)
    s = re.sub(r'\\property\s+([^. ]+)\s*\.\s*([^\\= ]+)\s*\\revert'
                 + r"\s*#'([-a-z0-9_]+)",
                 r"\\rev@rt \1.\2 #'\3", s)
    s = re.sub(r'Voice\.', '', s)
    s = re.sub(r'Lyrics\.', '', s)
    s = re.sub(r'ChordNames\.', '', s)

    s = re.sub('rev@rt', 'revert', s)
    s = re.sub('s@t', 'set', s)
    s = re.sub('overrid@', 'override', s)

    s = re.sub('molecule', 'stencil', s)
    s = re.sub('Molecule', 'Stencil', s)
    return s


@rule((2, 1, 23), r"Property setting syntax in \translator{ }")
def conv(s):
    def subst_in_trans(match):
        s = match.group(0)
        s = re.sub(r'\s([a-zA-Z]+)\s*\\override',
                   r' \\override \1', s)
        s = re.sub(r'\s([a-zA-Z]+)\s*\\set',
                   r' \\override \1', s)
        s = re.sub(r'\s([a-zA-Z]+)\s*\\revert',
                   r' \\revert \1', s)
        return s
    s = re.sub(r'\\(translator|with)\s*{[^}]+}',  subst_in_trans, s)

    def sub_abs(m):

        context = m.group('context')
        d = m.groupdict()
        if context:
            context = " '%s" % context[:-1]  # -1: remove .
        else:
            context = ''

        d['context'] = context

        return r"""#(override-auto-beam-setting %(prop)s %(num)s %(den)s%(context)s)""" % d

    s = re.sub(r"""\\override\s*(?P<context>[a-zA-Z]+\s*\.\s*)?autoBeamSettings"""
                 + r"""\s*#(?P<prop>[^=]+)\s*=\s*#\(ly:make-moment\s+(?P<num>\d+)\s+(?P<den>\d)\s*\)""",
                 sub_abs, s)
    return s


@rule((2, 1, 24), "music-list? -> ly:music-list?")
def conv(s):
    s = re.sub(r'music-list\?', 'ly:music-list?', s)
    s = re.sub(r'\|\s*~', '~ |', s)
    return s


@rule((2, 1, 25), "Scheme grob function renaming")
def conv(s):
    s = re.sub(r'ly:get-spanner-bound', 'ly:spanner-get-bound', s)
    s = re.sub(r'ly:get-extent', 'ly:grob-extent', s)
    s = re.sub(r'ly:get-system', 'ly:grob-system', s)
    s = re.sub(r'ly:get-original', 'ly:grob-original', s)
    s = re.sub(r'ly:get-parent', 'ly:grob-parent', s)
    s = re.sub(r'ly:get-broken-into', 'ly:spanner-broken-into', s)
    s = re.sub(r'Melisma_engraver', 'Melisma_translator', s)
    if re.search("ly:get-paper-variable", s):
        stderr_write(NOT_SMART % "ly:paper-get-variable")
        stderr_write(_('Use %s\n') % '(ly:paper-lookup (ly:grob-paper ))')
        raise FatalConversionError()

    s = re.sub(r'\\defaultAccidentals',
                 "#(set-accidental-style 'default)", s)
    s = re.sub(r'\\voiceAccidentals', "#(set-accidental-style 'voice)", s)
    s = re.sub(r'\\modernAccidentals',
                 "#(set-accidental-style 'modern)", s)
    s = re.sub(r'\\modernCautionaries',
                 "#(set-accidental-style 'modern-cautionary)", s)
    s = re.sub(r'\\modernVoiceAccidental',
                 "#(set-accidental-style 'modern-voice)", s)
    s = re.sub(r'\\modernVoiceCautionaries',
                 "#(set-accidental-style 'modern-voice-cautionary)", s)
    s = re.sub(r'\\pianoAccidentals', "#(set-accidental-style 'piano)", s)
    s = re.sub(r'\\pianoCautionaries',
                 "#(set-accidental-style 'piano-cautionary)", s)
    s = re.sub(r'\\forgetAccidentals',
                 "#(set-accidental-style 'forget)", s)
    s = re.sub(r'\\noResetKey', "#(set-accidental-style 'no-reset)", s)
    return s


@rule((2, 1, 26), "More Scheme function renaming")
def conv(s):
    s = re.sub('ly:set-grob-property!', 'ly:grob-set-property!', s)
    s = re.sub('ly:set-mus-property!', 'ly:music-set-property!', s)
    s = re.sub('ly:set-context-property!', 'ly:context-set-property!', s)
    s = re.sub('ly:get-grob-property', 'ly:grob-property', s)
    s = re.sub('ly:get-mus-property', 'ly:music-property', s)
    s = re.sub('ly:get-context-property', 'ly:context-property', s)
    return s


@rule((2, 1, 27), "property transposing -> tuning")
def conv(s):
    def subst(m):
        (o, g) = divmod(int(m.group(2)), 12)

        lower_pitches = [x for x in [0, 2, 4, 5, 7, 9, 11, 12] if x <= g]
        s = len(lower_pitches) - 1
        a = g - lower_pitches[-1]

        s = 'cdefgab' [s]
        s += ['eses', 'es', '', 'is', 'isis'][a + 2]
        o += 1                  # c' is octave 0
        if o < 0:
            s += (-o) * ","
        elif o > 0:
            s += o * "'"

        return '\\transposition %s ' % s

    s = re.sub(r"\\set ([A-Za-z]+\s*\.\s*)?transposing\s*=\s*#([-0-9]+)",
                 subst, s)
    return s


@rule((2, 1, 28), r"""make-music-by-name -> make-music,
new syntax for setting \arpeggioBracket""")
def conv(s):
    s = re.sub(r'make-music-by-name', 'make-music', s)
    s = re.sub(r"\\override\s+.*Arpeggio\s+#.print-function\s+=\s+\\arpeggioBracket",
                 r"\\arpeggioBracket", s)
    return s


@rule((2, 1, 29), r'\center -> \center-align, \translator -> \context')
def conv(s):
    s = re.sub(r'\\center([^-])', r'\\center-align\1', s)
    s = re.sub(r'\\translator', r'\\context', s)
    return s


@rule((2, 1, 30), r'''\threeq{flat,sharp} -> \sesqui{flat,sharp}
ly:get-mutable-properties -> ly:mutable-music-properties
centralCPosition -> middleCPosition
ly:unset-context-property -> ly:context-unset-property
ly:translator-find -> ly:context-find
ly:get-stencil-extent -> ly:stencil-extent
''')
def conv(s):
    s = re.sub(r'\\threeq(flat|sharp)', r'\\sesqui\1', s)
    s = re.sub(r'ly:stencil-get-extent',
                 'ly:stencil-extent', s)
    s = re.sub(r'ly:translator-find',
                 'ly:context-find', s)
    s = re.sub('ly:unset-context-property', 'ly:context-unset-property',
                 s)

    s = re.sub(r'ly:get-mutable-properties',
                 'ly:mutable-music-properties', s)
    s = re.sub(r'centralCPosition',
                 'middleCPosition', s)
    return s


@rule((2, 1, 31), r'remove \alias Timing')
def conv(s):
    s = re.sub(r'\\alias\s*"?Timing"?', '', s)
    return s


@rule((2, 1, 33), 'breakAlignOrder -> break-align-orders')
def conv(s):
    s = re.sub(r"(\\set\s+)?(?P<context>(Score\.)?)breakAlignOrder\s*=\s*#'(?P<list>[^\)]+)",
                 r"\n\\override \g<context>BreakAlignment #'break-align-orders = "
                 + r"#(make-vector 3 '\g<list>)", s)
    return s


@rule((2, 1, 34), 'set-paper-size -> set-default-paper-size')
def conv(s):
    s = re.sub(r"\(set-paper-size",
                 "(set-default-paper-size", s)
    return s


@rule((2, 1, 36), 'ly:mutable-music-properties -> ly:music-mutable-properties')
def conv(s):
    s = re.sub(r"ly:mutable-music-properties",
                 "ly:music-mutable-properties", s)
    return s


@rule((2, 2, 0), "bump version for release")
def conv(s):
    return s


@rule((2, 3, 1), r'\apply -> \applymusic')
def conv(s):
    return re.sub(r'\\apply\b', r'\\applymusic', s)


@rule((2, 3, 2), r'\FooContext -> \Foo')
def conv(s):
    if re.search('textheight', s):
        stderr_write(NOT_SMART % "textheight")
        stderr_write(UPDATE_MANUALLY)
        stderr_write(
            _("""Page layout has been changed, using paper size and margins.
textheight is no longer used.
"""))
    s = re.sub(r'\\OrchestralScoreContext', r'\\Score', s)

    def func(m):
        if m.group(1) not in ['RemoveEmptyStaff',
                              'AncientRemoveEmptyStaffContext',
                              'EasyNotation']:
            return '\\' + m.group(1)
        return m.group(0)

    s = re.sub(r'\\([a-zA-Z]+)Context\b', func, s)
    s = re.sub('ly:paper-lookup', 'ly:output-def-lookup', s)
    return s


@rule((2, 3, 4), r'remove \notes')
def conv(s):
    s = re.sub(r'\\notes\b', '', s)
    return s


@rule((2, 3, 6), 'lastpagefill -> raggedlastbottom')
def conv(s):
    s = re.sub(r'lastpagefill\s*=\s*"?1"', 'raggedlastbottom = ##t', s)
    return s


@rule((2, 3, 8), r'remove \consistsend, strip \lyrics from \lyricsto')
def conv(s):
    s = re.sub(r'\\consistsend', r'\\consists', s)
    s = re.sub(r'\\lyricsto\s+("?[a-zA-Z]+"?)(\s*\\new Lyrics\s*)?\\lyrics',
                 r'\\lyricsto \1 \2', s)
    return s


@rule((2, 3, 9), 'neo_mensural -> neomensural, if-text-padding -> bound-padding')
def conv(s):
    s = re.sub(r'neo_mensural', 'neomensural', s)
    s = re.sub(r'if-text-padding', 'bound-padding', s)
    return s


@rule((2, 3, 10), r'\addlyrics -> \oldaddlyrics, \newlyrics -> \addlyrics')
def conv(s):
    s = re.sub(r'\\addlyrics', r'\\oldaddlyrics', s)
    s = re.sub(r'\\newlyrics', r'\\addlyrics', s)
    if re.search(r"\\override\s*TextSpanner", s):
        stderr_write(
            "\nWarning: TextSpanner has been split into DynamicTextSpanner and TextSpanner\n")
    return s


@rule((2, 3, 11), r'\setMmRestFermata -> ^\fermataMarkup')
def conv(s):
    s = re.sub(r'\\setMmRestFermata\s+(R[0-9.*/]*)',
                 r'\1^\\fermataMarkup', s)
    return s


@rule((2, 3, 12), r'''\newpage -> \pageBreak, junk \script{up,down,both},
soloADue -> printPartCombineTexts, #notes-to-clusters -> \makeClusters
''')
def conv(s):
    s = re.sub(r'\\newpage', r'\\pageBreak', s)
    s = re.sub(r'\\scriptUp', r"""{
\\override TextScript  #'direction = #1
\\override Script  #'direction = #1
}""", s)
    s = re.sub(r'\\scriptDown', r"""{
  \\override TextScript  #'direction = #-1
  \\override Script  #'direction = #-1
}""", s)
    s = re.sub(r'\\scriptBoth', r"""{
  \\revert TextScript  #'direction
  \\revert Script  #'direction
}""", s)
    s = re.sub('soloADue', 'printPartCombineTexts', s)
    s = re.sub(r'\\applymusic\s*#notes-to-clusters',
                 r'\\makeClusters', s)

    s = re.sub(r'pagenumber\s*=', 'firstpagenumber = ', s)
    return s


@rule((2, 3, 16), r'''\foo -> \foomode (for chords, notes, etc.)
fold \new FooContext \foomode into \foo''')
def conv(s):
    s = re.sub(r'\\chords\b', r'\\chordmode', s)
    s = re.sub(r'\\lyrics\b', r'\\lyricmode', s)
    s = re.sub(r'\\figures\b', r'\\figuremode', s)
    s = re.sub(r'\\notes\b', r'\\notemode', s)
    s = re.sub(r'\\drums\b', r'\\drummode', s)
    s = re.sub(r'\\chordmode\s*\\new ChordNames', r'\\chords', s)
    s = re.sub(r'\\new ChordNames\s*\\chordmode', r'\\chords', s)
    s = re.sub(r'\\new FiguredBass\s*\\figuremode', r'\\figures', s)
    s = re.sub(r'\\figuremode\s*\\new FiguredBass', r'\\figures', s)
    s = re.sub(r'\\new DrumStaff\s*\\drummode', r'\\drums', s)
    s = re.sub(r'\\drummode\s*\\new DrumStaff', r'\\drums', s)

    return s


@rule((2, 3, 17), r'''slurBoth -> slurNeutral, stemBoth -> stemNeutral, etc.
\applymusic #(remove-tag 'foo) -> \removeWithTag 'foo''')
def conv(s):
    s = re.sub(
        r'(slur|stem|phrasingSlur|tie|dynamic|dots|tuplet|arpeggio|)Both', r'\1Neutral', s)
    s = re.sub(r"\\applymusic\s*#\(remove-tag\s*'([a-z-0-9]+)\)",
                 r"\\removeWithTag #'\1", s)
    return s


@rule((2, 3, 18), 'Text_item -> Text_interface')
def conv(s):
    s = re.sub(r'Text_item', 'Text_interface', s)
    return s


@rule((2, 3, 22), 'paper -> layout, bookpaper -> paper')
def conv(s):
    s = re.sub(r'\\paper', r'\\layout', s)
    s = re.sub(r'\\bookpaper', r'\\paper', s)
    if re.search('paper-set-staff-size', s):
        warning(_('''staff size should be changed at top-level
with

  #(set-global-staff-size <STAFF-HEIGHT-IN-POINT>)

'''))

    s = re.sub(r'#\(paper-set-staff-size',
                 '%Use set-global-staff-size at toplevel\n% #(layout-set-staff-size', s)
    return s


@rule((2, 3, 23), r'\context Foo = NOTENAME -> \context Foo = "NOTENAME"')
def conv(s):
    s = re.sub(r'\\context\s+([a-zA-Z]+)\s*=\s*([a-z]+)\s',
                 r'\\context \1 = "\2" ',
                 s)
    return s


@rule((2, 3, 24), '''regularize other identifiers''')
def conv(s):
    def sub(m):
        return regularize_id(m.group(1))
    s = re.sub(r'(maintainer_email|maintainer_web|midi_stuff|gourlay_maxmeasures)',
                 sub, s)
    return s


@rule((2, 3, 25), 'petrucci_c1 -> petrucci-c1, 1style -> single-digit')
def conv(s):
    s = re.sub('petrucci_c1', 'petrucci-c1', s)
    s = re.sub('1style', 'single-digit', s)
    return s


@rule((2, 4, 0), "bump version for release")
def conv(s):
    return s


@rule((2, 5, 0), r'\quote -> \quoteDuring')
def conv(s):
    s = re.sub(r'\\quote\s+"?([a-zA-Z0-9]+)"?\s+([0-9.*/]+)',
                 r'\\quoteDuring #"\1" { \\skip \2 }',
                 s)
    return s


@rule((2, 5, 1), 'ly:import-module -> ly:module-copy')
def conv(s):
    s = re.sub(r'ly:import-module',
                 r'ly:module-copy', s)
    return s


@rule((2, 5, 2), r'\markup .. < .. > .. -> \markup .. { .. } ..')
def conv(s):
    s = re.sub(r'\\(column|fill-line|dir-column|center-align|right-align|left-align|bracketed-y-column)\s*<(([^>]|<[^>]*>)*)>',
                 r'\\\1 {\2}', s)
    s = re.sub(r'\\(column|fill-line|dir-column|center-align|right-align|left-align|bracketed-y-column)\s*<(([^>]|<[^>]*>)*)>',
                 r'\\\1 {\2}', s)
    s = re.sub(r'\\(column|fill-line|dir-column|center-align|right-align|left-align|bracketed-y-column)\s*<(([^>]|<[^>]*>)*)>',
                 r'\\\1 {\2}', s)

    def get_markup(m):
        s = m.group(0)
        s = re.sub(r'''((\\"|})\s*){''', r'\2 \\line {', s)
        return s
    s = re.sub(r'\\markup\s*{([^}]|{[^}]*})*}', get_markup, s)
    return s


@rule((2, 5, 3), 'ly:find-glyph-by-name -> ly:font-get-glyph, remove - from glyphnames.')
def conv(s):
    s = re.sub('ly:find-glyph-by-name', 'ly:font-get-glyph', s)
    s = re.sub('"(scripts|clefs|accidentals)-', r'"\1.', s)
    s = re.sub("'hufnagel-do-fa", "'hufnagel.do.fa", s)
    s = re.sub(
        "'(vaticana|hufnagel|medicaea|petrucci|neomensural|mensural)-", r"'\1.", s)
    return s


@rule((2, 5, 12), r"\set Slur #'dashed = #X -> \slurDashed")
def conv(s):
    s = re.sub(r"\\override\s+(Voice\.)?Slur #'dashed\s*=\s*#\d*(\.\d+)?",
                 r"\\slurDashed", s)
    return s


@rule((2, 5, 13), r'''\encoding: smart recode latin1..utf-8.
Remove ly:point-and-click''')
def conv(s):
    def func(match):
        encoding = match.group(1)

        if encoding == 'latin1':
            return match.group(2)

        stderr_write(NOT_SMART % ("\\encoding: %s" % encoding))
        stderr_write(_("LilyPond source must be UTF-8"))
        stderr_write('\n')
        if encoding == 'TeX':
            stderr_write(_("Try the texstrings backend"))
            stderr_write('\n')
        else:
            stderr_write(_("Do something like: %s") %
                         ("recode %s..utf-8 FILE" % encoding))
            stderr_write('\n')
        stderr_write(_("Or save as UTF-8 in your editor"))
        stderr_write('\n')
        raise FatalConversionError()

    s = re.sub(r'\\encoding\s+"?([a-zA-Z0-9]+)"?(\s+)', func, s)

    s = re.sub(r"#\(ly:set-point-and-click '[a-z-]+\)", '', s)
    return s


@rule((2, 5, 17), 'remove ly:stencil-set-extent!')
def conv(s):
    if re.search("ly:stencil-set-extent!", s):
        stderr_write(NOT_SMART % "ly:stencil-set-extent!")
        stderr_write(_('Use %s\n') %
                     '(set! VAR (ly:make-stencil (ly:stencil-expr VAR) X-EXT Y-EXT))')
        raise FatalConversionError()
    if re.search("ly:stencil-align-to!", s):
        stderr_write(NOT_SMART % "ly:stencil-align-to!")
        stderr_write(_('Use %s\n') %
                     '(set! VAR (ly:stencil-aligned-to VAR AXIS DIR))')
        raise FatalConversionError()
    return s


@rule((2, 5, 18), 'ly:warn -> ly:warning')
def conv(s):
    s = re.sub(r"ly:warn\b", 'ly:warning', s)
    return s


@rule((2, 5, 21), 'warn about auto beam settings')
def conv(s):
    if re.search("(override-|revert-)auto-beam-setting", s)\
       or re.search("autoBeamSettings", s):
        stderr_write(NOT_SMART % _("auto beam settings"))
        stderr_write(_('''
Auto beam settings must now specify each interesting moment in a measure
explicitly; 1/4 is no longer multiplied to cover moments 1/2 and 3/4 too.
'''))
        stderr_write(UPDATE_MANUALLY)
        raise FatalConversionError()
    return s


@rule((2, 5, 25), 'unfoldrepeats -> unfoldRepeats, compressmusic -> compressMusic')
def conv(s):
    s = re.sub(r"unfoldrepeats", 'unfoldRepeats', s)
    s = re.sub(r"compressmusic", 'compressMusic', s)
    return s


@rule((2, 6, 0), "bump version for release")
def conv(s):
    return s


@rule((2, 7, 0), 'ly:get-default-font -> ly:grob-default-font')
def conv(s):
    return re.sub('ly:get-default-font', 'ly:grob-default-font', s)


@rule((2, 7, 1), '''ly:parser-define -> ly:parser-define!
excentricity -> eccentricity
Timing_engraver -> Timing_translator + Default_bar_line_engraver
''')
def conv(s):
    s = re.sub('ly:parser-define', 'ly:parser-define!', s)
    s = re.sub('excentricity', 'eccentricity', s)
    s = re.sub(r'\\(consists|remove) *"?Timing_engraver"?',
                 r'\\\1 "Timing_translator" \\\1 "Default_bar_line_engraver"',
                 s)
    return s


@rule((2, 7, 2), 'ly:X-moment -> ly:moment-X')
def conv(s):
    s = re.sub('ly:(add|mul|mod|div)-moment', r'ly:moment-\1', s)
    return s


@rule((2, 7, 4), 'keyAccidentalOrder -> keyAlterationOrder')
def conv(s):
    s = re.sub('keyAccidentalOrder', 'keyAlterationOrder', s)
    return s


@rule((2, 7, 6), '''Performer_group_performer -> Performer_group, Engraver_group_engraver -> Engraver_group,
inside-slur -> avoid-slur''')
def conv(s):
    s = re.sub('Performer_group_performer', 'Performer_group', s)
    s = re.sub('Engraver_group_engraver', 'Engraver_group', s)
    s = re.sub(r"#'inside-slur\s*=\s*##t *",
                 r"#'avoid-slur = #'inside ", s)
    s = re.sub(r"#'inside-slur\s*=\s*##f *",
                 r"#'avoid-slur = #'around ", s)
    s = re.sub(r"#'inside-slur",
                 r"#'avoid-slur", s)
    return s


@rule((2, 7, 10), r'\applyxxx -> \applyXxx')
def conv(s):
    s = re.sub(r'\\applyoutput', r'\\applyOutput', s)
    s = re.sub(r'\\applycontext', r'\\applyContext', s)
    s = re.sub(r'\\applymusic', r'\\applyMusic', s)
    s = re.sub(r'ly:grob-suicide', 'ly:grob-suicide!', s)
    return s


@rule((2, 7, 11), '"tabloid" -> "11x17"')
def conv(s):
    s = re.sub('"tabloid"', '"11x17"', s)
    return s


@rule((2, 7, 12), 'outputProperty -> overrideProperty')
def conv(s):
    s = re.sub(r'outputProperty', 'overrideProperty', s)
    return s


@rule((2, 7, 13), 'layout engine refactoring [FIXME]')
def conv(s):
    def subber(match):
        newkey = {'spacing-procedure': 'springs-and-rods',
                  'after-line-breaking-callback': 'after-line-breaking',
                  'before-line-breaking-callback': 'before-line-breaking',
                  'print-function': 'stencil'}[match.group(3)]
        what = match.group(1)
        grob = match.group(2)

        if what == 'revert':
            return "revert %s #'callbacks %% %s\n" % (grob, newkey)
        if what == 'override':
            return "override %s #'callbacks #'%s" % (grob, newkey)
        raise RuntimeError('1st group should match revert or override')

    s = re.sub(r"(override|revert)\s*([a-zA-Z.]+)\s*#'(spacing-procedure|after-line-breaking-callback"
                 + r"|before-line-breaking-callback|print-function)",
                 subber, s)

    if re.search('bar-size-procedure', s):
        stderr_write(NOT_SMART % "bar-size-procedure")
    if re.search('space-function', s):
        stderr_write(NOT_SMART % "space-function")
    if re.search('verticalAlignmentChildCallback', s):
        stderr_write(_('verticalAlignmentChildCallback has been deprecated'))
        stderr_write('\n')
    return s


@rule((2, 7, 14), 'Remove callbacks property, deprecate XY-extent-callback')
def conv(s):
    s = re.sub(r"\\override +([A-Z.a-z]+) #'callbacks",
                 r"\\override \1", s)
    s = re.sub(r"\\revert ([A-Z.a-z]+) #'callbacks % ([a-zA-Z]+)",
                 r"\\revert \1 #'\2", s)
    s = re.sub(r"([XY]-extent)-callback", r'\1', s)
    s = re.sub(r"RemoveEmptyVerticalGroup", "VerticalAxisGroup", s)
    s = re.sub(r"\\set ([a-zA-Z]*\.?)minimumVerticalExtent",
                 r"\\override \1VerticalAxisGroup #'minimum-Y-extent",
                 s)
    s = re.sub(r"minimumVerticalExtent",
                 r"\\override VerticalAxisGroup #'minimum-Y-extent",
                 s)
    s = re.sub(r"\\set ([a-zA-Z]*\.?)extraVerticalExtent",
                 r"\\override \1VerticalAxisGroup #'extra-Y-extent", s)
    s = re.sub(r"\\set ([a-zA-Z]*\.?)verticalExtent",
                 r"\\override \1VerticalAxisGroup #'Y-extent", s)
    return s


@rule((2, 7, 15), 'Use grob closures iso. XY-offset-callbacks')
def conv(s):
    if re.search('[XY]-offset-callbacks', s):
        stderr_write(NOT_SMART % "[XY]-offset-callbacks")
    if re.search('position-callbacks', s):
        stderr_write(NOT_SMART % "position-callbacks")
    return s


@rule((2, 7, 18), r"""bassFigureFormatFunction -> figuredBassFormatter
deprecate alignBassFigureAccidentals
""")
def conv(s):
    s = re.sub('bassFigureFormatFunction', 'figuredBassFormatter', s)
    if re.search('alignBassFigureAccidentals', s):
        stderr_write(NOT_SMART % "alignBassFigureAccidentals")
    return s


@rule((2, 7, 22), r"\tag #'(a b) -> \tag #'a \tag #'b")
def conv(s):
    def sub_syms(m):
        syms = m.group(1).split()
        tags = ["\\tag #'%s" % s for s in syms]
        return ' '.join(tags)

    s = re.sub(r"\\tag #'\(([^)]+)\)", sub_syms, s)
    return s


@rule((2, 7, 24), 'deprecate number-visibility')
def conv(s):
    s = re.sub(r"#'number-visibility",
                 "#'number-visibility % number-visibility is deprecated. Tune the TupletNumber instead\n",
                 s)
    return s


@rule((2, 7, 28), "ly:spanner-get-bound -> ly:spanner-bound")
def conv(s):
    s = re.sub(r"ly:spanner-get-bound", "ly:spanner-bound", s)
    return s


@rule((2, 7, 29), "override Stem #'beamed-* -> #'details #'beamed-*")
def conv(s):
    for a in ['beamed-lengths', 'beamed-minimum-free-lengths',
              'lengths',
              'beamed-extreme-minimum-free-lengths']:
        s = re.sub(r"\\override\s+Stem\s+#'%s" % a,
                     r"\\override Stem #'details #'%s" % a,
                     s)
    return s


@rule((2, 7, 30), r"\epsfile")
def conv(s):
    s = re.sub(r'\\epsfile *#"', r'\\epsfile #X #10 #"', s)
    return s


@rule((2, 7, 31), "Foo_bar::bla_bla -> ly:foo-bar::bla-bla")
def conv(s):
    def sub_cxx_id(m):
        s = m.group(1)
        return 'ly:' + s.lower().replace('_', '-')

    s = re.sub(r'([A-Z][a-z_0-9]+::[a-z_0-9]+)',
                 sub_cxx_id, s)
    return s


@rule((2, 7, 32), r"foobar -> foo-bar for \paper, \layout")
def conv(s):
    identifier_subs = [
        ('inputencoding', 'input-encoding'),
        ('printpagenumber', 'print-page-number'),
        ('outputscale', 'output-scale'),
        ('betweensystemspace', 'between-system-space'),
        ('betweensystempadding', 'between-system-padding'),
        ('pagetopspace', 'page-top-space'),
        ('raggedlastbottom', 'ragged-last-bottom'),
        ('raggedright', 'ragged-right'),
        ('raggedlast', 'ragged-last'),
        ('raggedbottom', 'ragged-bottom'),
        ('aftertitlespace', 'after-title-space'),
        ('beforetitlespace', 'before-title-space'),
        ('betweentitlespace', 'between-title-space'),
        ('topmargin', 'top-margin'),
        ('bottommargin', 'bottom-margin'),
        ('headsep', 'head-separation'),
        ('footsep', 'foot-separation'),
        ('rightmargin', 'right-margin'),
        ('leftmargin', 'left-margin'),
        ('printfirstpagenumber', 'print-first-page-number'),
        ('firstpagenumber', 'first-page-number'),
        ('hsize', 'paper-width'),
        ('vsize', 'paper-height'),
        ('horizontalshift', 'horizontal-shift'),
        ('staffspace', 'staff-space'),
        ('linethickness', 'line-thickness'),
        ('ledgerlinethickness', 'ledger-line-thickness'),
        ('blotdiameter', 'blot-diameter'),
        ('staffheight', 'staff-height'),
        ('linewidth', 'line-width'),
        ('annotatespacing', 'annotate-spacing')
    ]

    for (a, b) in identifier_subs:
        # for C++:
        # s = re.sub ('"%s"' % a, '"%s"' b, s)

        s = re.sub(a, b, s)
    return s


@rule((2, 7, 32), "debug-beam-quanting -> debug-beam-scoring")
def conv(s):
    s = re.sub('debug-beam-quanting', 'debug-beam-scoring', s)
    return s


@rule((2, 7, 36), "def-(music-function|markup-command) -> define-(music-function|markup-command)")
def conv(s):
    s = re.sub('def-music-function', 'define-music-function', s)
    s = re.sub('def-markup-command', 'define-markup-command', s)
    return s


@rule((2, 7, 40), "rehearsalMarkAlignSymbol/barNumberAlignSymbol -> break-align-symbol")
def conv(s):
    s = re.sub(r'\\set\s+Score\s*\.\s*barNumberAlignSymbol\s*=',
                 r"\\override Score.BarNumber #'break-align-symbol = ", s)
    s = re.sub(r'\\set\s*Score\s*\.\s*rehearsalMarkAlignSymbol\s*=',
                 r"\\override Score.RehearsalMark #'break-align-symbol = ", s)
    return s


@rule((2, 9, 4), "(page-)penalty -> (page-)break-penalty")
def conv(s):
    s = re.sub('page-penalty', 'page-break-penalty', s)
    s = re.sub('([^-])penalty', '\1break-penalty', s)
    return s


@rule((2, 9, 6), r"\context Foo \applyOutput #bla -> \applyOutput #'Foo #bla ")
def conv(s):
    s = re.sub(
        r'\\context\s+"?([a-zA-Z]+)"?\s*\\applyOutput', r"\\applyOutput #'\1", s)
    return s


@rule((2, 9, 9), "annotatefoo -> annotate-foo")
def conv(s):
    s = re.sub('annotatepage', 'annotate-page', s)
    s = re.sub('annotateheaders', 'annotate-headers', s)
    s = re.sub('annotatesystems', 'annotate-systems', s)
    return s


@rule((2, 9, 11), r"\set tupletNumberFormatFunction -> \override #'text = ")
def conv(s):
    s = re.sub(r"""(\\set\s)?(?P<context>[a-zA-Z]*.?)tupletNumberFormatFunction\s*=\s*#denominator-tuplet-formatter""",
                 r"""\\override \g<context>TupletNumber #'text = #tuplet-number::calc-denominator-text""", s)

    s = re.sub(r"""(\\set\s+)?(?P<context>[a-zA-Z]*.?)tupletNumberFormatFunction\s*=\s*#fraction-tuplet-formatter""",
                 r"""\\override \g<context>TupletNumber #'text = #tuplet-number::calc-fraction-text""", s)

    if re.search('tupletNumberFormatFunction', s):
        stderr_write("\n")
        stderr_write(
            "tupletNumberFormatFunction has been removed. Use #'text property on TupletNumber")
        stderr_write("\n")
    return s


@rule((2, 9, 13), "instrument -> instrumentName, instr -> shortInstrumentName, vocNam -> shortVocalName")
def conv(s):
    s = re.sub('vocNam', 'shortVocalName', s)
    s = re.sub(r'\.instr\s*=', r'.shortInstrumentName =', s)
    s = re.sub(r'\.instrument\s*=', r'.instrumentName =', s)
    return s


@rule((2, 9, 16), r"deprecate \tempo in \midi")
def conv(s):

    def sub_tempo(m):
        dur = int(m.group(1))
        dots = len(m.group(2))
        count = int(m.group(3))

        log2 = 0
        while dur > 1:
            dur /= 2
            log2 += 1

        den = (1 << dots) * (1 << log2)
        num = ((1 << (dots+1)) - 1)

        return r"""
  \midi {
    \context {
      \Score
      tempoWholesPerMinute = #(ly:make-moment %d %d)
      }
    }

""" % (num*count, den)

    s = re.sub(
        r'\\midi\s*{\s*\\tempo ([0-9]+)\s*([.]*)\s*=\s*([0-9]+)\s*}', sub_tempo, s)
    return s


@rule((2, 9, 19), "printfirst-page-number -> print-first-page-number")
def conv(s):
    s = re.sub('printfirst-page-number', 'print-first-page-number', s)
    return s


@rule((2, 10, 0), "bump version for release")
def conv(s):
    return s


@rule((2, 11, 2), "ly:clone-parser -> ly:parser-clone")
def conv(s):
    return re.sub('ly:clone-parser',
                  'ly:parser-clone', s)


@rule((2, 11, 3), "no-spacing-rods -> extra-spacing-width")
def conv(s):
    s = re.sub(r"no-spacing-rods\s+=\s+##t",
                 r"extra-spacing-width = #'(+inf.0 . -inf.0)", s)
    s = re.sub(r"no-spacing-rods\s+=\s+##f",
                 r"extra-spacing-width = #'(0 . 0)", s)
    return s


@rule((2, 11, 5), """deprecate cautionary-style.
Use AccidentalCautionary properties""")
def conv(s):
    s = re.sub(r"Accidental\s*#'cautionary-style\s*=\s*#'smaller",
                 "AccidentalCautionary #'font-size = #-2", s)
    s = re.sub(r"Accidental\s*#'cautionary-style\s*=\s*#'parentheses",
                 "AccidentalCautionary #'parenthesized = ##t", s)
    s = re.sub(r"([A-Za-z]+)\s*#'cautionary-style\s*=\s*#'parentheses",
                 r"\1 #'parenthesized = ##t", s)
    s = re.sub(r"([A-Za-z]+)\s*#'cautionary-style\s*=\s*#'smaller",
                 r"\1 #'font-size = #-2", s)
    return s


@rule((2, 11, 6), "Rename accidental glyphs, use glyph-name-alist")
def conv(s):

    def sub_acc_name(m):
        idx = int(m.group(1).replace('M', '-'))

        return ["accidentals.doublesharp",
                "accidentals.sharp.slashslash.stemstemstem",
                "accidentals.sharp",
                "accidentals.sharp.slashslash.stem",
                "accidentals.natural",
                "accidentals.mirroredflat",
                "accidentals.flat",
                "accidentals.mirroredflat.flat",
                "accidentals.flatflat"][4-idx]

    s = re.sub(r"accidentals[.](M?[-0-9]+)",
                 sub_acc_name, s)
    s = re.sub(r"(KeySignature|Accidental[A-Za-z]*)\s*#'style\s*=\s*#'([a-z]+)",
                 r"\1 #'glyph-name-alist = #alteration-\2-glyph-name-alist", s)
    # FIXME: standard vs default, alteration-FOO vs FOO-alteration
    s = s.replace('alteration-default-glyph-name-alist',
                      'standard-alteration-glyph-name-alist')
    return s


@rule((2, 11, 10), """allowBeamBreak -> Beam #'breakable = ##t
addquote -> addQuote
""")
def conv(s):
    s = re.sub(r'(\\set\s+)?([A-Z][a-zA-Z]+\s*\.\s*)allowBeamBreak',
                 r"\\override \2Beam #'breakable", s)
    s = re.sub(r'(\\set\s+)?allowBeamBreak',
                 r"\\override Beam #'breakable", s)
    s = re.sub(r'addquote', 'addQuote', s)
    if re.search("Span_dynamic_performer", s):
        stderr_write(
            "Span_dynamic_performer has been merged into Dynamic_performer")

    return s


@rule((2, 11, 11), "layout-set-staff-size -> layout-set-absolute-staff-size")
def conv(s):
    s = re.sub(r'\(layout-set-staff-size \(\*\s*([0-9.]+)\s*(pt|mm|cm)\)\)',
                 r'(layout-set-absolute-staff-size (* \1 \2))', s)
    return s


@rule((2, 11, 13), "#'arrow = ##t -> #'bound-details #'right #'arrow = ##t")
def conv(s):
    s = re.sub(r"\\override\s*([a-zA-Z.]+)\s*#'arrow\s*=\s*##t",
                 r"\\override \1 #'bound-details #'right #'arrow = ##t",
                 s)

    if re.search('edge-text', s):
        stderr_write(NOT_SMART % _("edge-text settings for TextSpanner"))
        stderr_write(_("Use\n\n%s") %
                     "\t\\override TextSpanner #'bound-details #'right #'text = <right-text>\n"
                     "\t\\override TextSpanner #'bound-details #'left #'text = <left-text>\n")
    return s


@rule((2, 11, 15), """TextSpanner #'edge-height -> #'bound-details #'right/left #'text = ...
Remove 'forced-distance for fixed spacing between staves in a PianoStaff""")
def conv(s):
    def sub_edge_height(m):
        s = ''
        for (var, h) in [('left', m.group(3)),
                         ('right', m.group(4))]:

            if h and float(h):
                once = m.group(1)
                if not once:
                    once = ''
                context = m.group(2)
                if not context:
                    context = ''

                s += (r"%s \override %sTextSpanner #'bound-details #'%s #'text = \markup { \draw-line #'(0 . %s) }"
                      % (once, context, var, h))

                s += '\n'

        return s

    s = re.sub(
        r"(\\once)?\s*\\override\s*([a-zA-Z]+\s*[.]\s*)?TextSpanner\s*#'edge-height\s*=\s*#'\(\s*([0-9.-]+)\s+[.]\s+([0-9.-]+)\s*\)", sub_edge_height, s)
    if re.search(r"#'forced-distance", s):
        stderr_write(NOT_SMART % "VerticalAlignment #'forced-distance")
        stderr_write(_("Use the `alignment-offsets' sub-property of\n"))
        stderr_write(_("NonMusicalPaperColumn #'line-break-system-details\n"))
        stderr_write(_("to set fixed distances between staves.\n"))
    return s


@rule((2, 11, 23), "#'break-align-symbol -> #'break-align-symbols")
def conv(s):
    s = re.sub(r"\\override\s*([a-zA-Z.]+)\s*#'break-align-symbol\s*=\s*#'([a-z-]+)",
                 r"\\override \1 #'break-align-symbols = #'(\2)", s)
    return s


@rule((2, 11, 35), """scripts.caesura -> scripts.caesura.curved.
Use #'style not #'dash-fraction to select solid/dashed lines""")
def conv(s):
    s = re.sub(r"scripts\.caesura",
                 r"scripts.caesura.curved", s)

    if re.search('dash-fraction', s):
        stderr_write(NOT_SMART % _("all settings related to dashed lines"))
        stderr_write(
            _("Use \\override ... #'style = #'line for solid lines and\n"))
        stderr_write(
            _("\t\\override ... #'style = #'dashed-line for dashed lines."))
    return s


@rule((2, 11, 38), r"""\setEasyHeads -> \easyHeadsOn, \fatText -> \textLengthOn,
\emptyText -> \textLengthOff""")
def conv(s):
    s = re.sub(r"setEasyHeads", r"easyHeadsOn", s)
    s = re.sub(r"fatText", r"textLengthOn", s)
    s = re.sub(r"emptyText", r"textLengthOff", s)
    return s


@rule((2, 11, 46), r"\set hairpinToBarline -> \override Hairpin #'to-barline")
def conv(s):
    s = re.sub(r"\\set\s+([a-zA-Z]+)\s*.\s*hairpinToBarline\s*=\s*##([tf]+)",
                 r"\\override \1.Hairpin #'to-barline = ##\2", s)
    s = re.sub(r"\\set\s+hairpinToBarline\s*=\s*##([tf]+)",
                 r"\\override Hairpin #'to-barline = ##\1", s)
    s = re.sub(r"\\unset\s+([a-zA-Z]+)\s*.\s*hairpinToBarline",
                 r"\\revert \1.Hairpin #'to-barline", s)
    s = re.sub(r"\\unset\s+hairpinToBarline",
                 r"\\revert Hairpin #'to-barline", s)
    s = re.sub(r"hairpinToBarline\s*=\s*##([tf]+)",
                 r"\\override Hairpin #'to-barline = ##\1", s)
    s = re.sub(r"\\set (de|)crescendoSpanner = #'dashed-line",
                 r"\\set \1crescendoSpanner = #'text", s)
    return s


@rule((2, 11, 48), r"\compressMusic -> \scaleDurations")
def conv(s):
    s = re.sub(r"compressMusic", r"scaleDurations", s)
    return s


@rule((2, 11, 50), """metronomeMarkFormatter uses text markup as second argument,
fret diagram properties moved to fret-diagram-details""")
def conv(s):
    # warning 1/2: metronomeMarkFormatter uses text markup as second argument
    if re.search('metronomeMarkFormatter', s):
        stderr_write(NOT_SMART % "metronomeMarkFormatter")
        stderr_write(
            _("metronomeMarkFormatter got an additional text argument.\n"))
        stderr_write(_("The function assigned to Score.metronomeMarkFunction now uses the signature\n%s") %
                     "\t(format-metronome-markup text dur count context)\n")

    # warning 2/2: fret diagram properties moved to fret-diagram-details
    fret_props = ['barre-type',
                  'dot-color',
                  'dot-radius',
                  'finger-code',
                  'fret-count',
                  'label-dir',
                  'number-type',
                  'string-count',
                  'xo-font-magnification',
                  'mute-string',
                  'open-string',
                  'orientation']
    for prop in fret_props:
        if re.search(prop, s):
            stderr_write(NOT_SMART %
                         (_("%s in fret-diagram properties") % prop))
            stderr_write(_('Use %s\n') % "fret-diagram-details")
    return s


@rule((2, 11, 51), r"""\octave -> \octaveCheck, \arpeggioUp -> \arpeggioArrowUp,
\arpeggioDown -> \arpeggioArrowDown, \arpeggioNeutral -> \arpeggioNormal,
\setTextCresc -> \crescTextCresc, \setTextDecresc -> \dimTextDecresc,
\setTextDecr -> \dimTextDecr, \setTextDim -> \dimTextDim,
\setHairpinCresc -> \crescHairpin, \setHairpinDecresc -> \dimHairpin,
\sustainUp -> \sustainOff, \sustainDown -> \sustainOn
\sostenutoDown -> \sostenutoOn, \sostenutoUp -> \sostenutoOff""")
def conv(s):
    s = re.sub(r"\\octave(?![a-zA-Z])", r"\\octaveCheck", s)
    s = re.sub(r"arpeggioUp", r"arpeggioArrowUp", s)
    s = re.sub(r"arpeggioDown", r"arpeggioArrowDown", s)
    s = re.sub(r"arpeggioNeutral", r"arpeggioNormal", s)
    s = re.sub(r"setTextCresc", r"crescTextCresc", s)
    s = re.sub(r"setTextDecresc", r"dimTextDecresc", s)
    s = re.sub(r"setTextDecr", r"dimTextDecr", s)
    s = re.sub(r"setTextDim", r"dimTextDim", s)
    s = re.sub(r"setHairpinCresc", r"crescHairpin", s)
    s = re.sub(r"setHairpinDecresc", r"dimHairpin", s)
    s = re.sub(r"sustainUp", r"sustainOff", s)
    s = re.sub(r"sustainDown", r"sustainOn", s)
    s = re.sub(r"sostenutoDown", r"sostenutoOn", s)
    s = re.sub(r"sostenutoUp", r"sostenutoOff", s)
    return s


@rule((2, 11, 52), r"\setHairpinDim -> \dimHairpin")
def conv(s):
    s = s.replace("setHairpinDim", "dimHairpin")
    return s


@rule((2, 11, 53), "infinite-spacing-height -> extra-spacing-height")
def conv(s):
    s = re.sub(r"infinite-spacing-height\s+=\s+##t",
                 r"extra-spacing-height = #'(-inf.0 . +inf.0)", s)
    s = re.sub(r"infinite-spacing-height\s+=\s+##f",
                 r"extra-spacing-height = #'(0 . 0)", s)
    return s


@rule((2, 11, 55), r"""#(set-octavation oct) -> \ottava #oct,
\put-adjacent markup axis dir markup -> \put-adjacent axis dir markup markup""")
def conv(s):
    s = re.sub(r"#\(set-octavation (-*[0-9]+)\)", r"\\ottava #\1", s)
    if re.search('put-adjacent', s):
        stderr_write(NOT_SMART % _("\\put-adjacent argument order"))
        stderr_write(_("Axis and direction now come before markups:\n"))
        stderr_write(_("\\put-adjacent axis dir markup markup."))
        stderr_write("\n")
    return s


@rule((2, 11, 57), r"\center-align -> \center-column, \hcenter -> \center-align")
def conv(s):
    s = re.sub(r"([\\:]+)center-align", r"\1center-column", s)
    s = re.sub(r"hcenter(\s+)", r"center-align\1", s)
    return s


@rule((2, 11, 60), "printallheaders -> print-all-headers")
def conv(s):
    s = re.sub(r"printallheaders", r"print-all-headers", s)
    return s


@rule((2, 11, 61), "gregorian-init.ly -> gregorian.ly")
def conv(s):
    s = re.sub(r'\\include(\s+)"gregorian-init.ly"',
                 r'\\include\1"gregorian.ly"', s)
    return s


@rule((2, 11, 62), r"makam-init.ly -> makam.ly, \bigger -> \larger")
def conv(s):
    s = re.sub(r'\\include(\s+)"makam-init.ly"',
                 r'\\include\1"makam.ly"', s)
    s = re.sub(r"([\\:])bigger", r"\1larger", s)
    return s


@rule((2, 11, 64), """systemSeparatorMarkup -> system-separator-markup,
InnerStaffGroup -> StaffGroup, InnerChoirStaff -> ChoirStaff""")
def conv(s):
    s = re.sub(r'systemSeparatorMarkup', r'system-separator-markup', s)
    if re.search(r'\\InnerStaffGroup', s):
        stderr_write(NOT_SMART % _("re-definition of InnerStaffGroup"))
        stderr_write(FROM_TO % ("InnerStaffGroup", "StaffGroup"))
        stderr_write(UPDATE_MANUALLY)
        raise FatalConversionError()
    if re.search(r'\\InnerChoirStaff', s):
        stderr_write(NOT_SMART % _("re-definition of InnerChoirStaff"))
        stderr_write(FROM_TO % ("InnerChoirStaff", "ChoirStaff"))
        stderr_write(UPDATE_MANUALLY)
        raise FatalConversionError()

    s = re.sub('InnerStaffGroup', 'StaffGroup', s)
    s = re.sub('InnerChoirStaff', 'ChoirStaff', s)
    return s


@rule((2, 12, 0), r"""Syntax changes for \addChordShape and \chord-shape
bump version for release""")
def conv(s):
    if re.search(r'\\addChordShape', s):
        stderr_write(NOT_SMART % "addChordShape")
        stderr_write(_("stringTuning must be added to addChordShape call.\n"))
        stderr_write(UPDATE_MANUALLY)
        raise FatalConversionError()
    if re.search(r'\\chord-shape', s):
        stderr_write(NOT_SMART % "chord-shape")
        stderr_write(_("stringTuning must be added to chord-shape call.\n"))
        stderr_write(UPDATE_MANUALLY)
        raise FatalConversionError()
    return s


@rule((2, 12, 3), "Remove oldaddlyrics")
def conv(s):
    if re.search(r'\\oldaddlyrics', s):
        stderr_write(NOT_SMART % "oldaddlyrics")
        stderr_write(_("oldaddlyrics is no longer supported. \n \
        Use addlyrics or lyricsto instead.\n"))
        stderr_write(UPDATE_MANUALLY)
        raise FatalConversionError()
    return s


@rule((2, 13, 0), """keySignature property not reversed any more
MIDI 47: orchestral strings -> orchestral harp""")
def conv(s):
    if re.search(r'\\set Staff.keySignature', s):
        stderr_write(NOT_SMART % "Staff.keySignature")
        stderr_write(_("The alist for Staff.keySignature is no \
longer in reversed order.\n"))
    s = s.replace('"orchestral strings"', '"orchestral harp"')
    return s


@rule((2, 13, 1), r"""\bar "." now produces a thick barline
ly:hairpin::after-line-breaking -> ly:spanner::kill-zero-spanned-time
Dash parameters for slurs and ties are now in dash-definition""")
def conv(s):
    if re.search(r'\\bar\s*"\."', s):
        stderr_write(NOT_SMART % "\\bar \".\"")
        stderr_write(_("\\bar \".\" now produces a thick barline.\n"))
        stderr_write(UPDATE_MANUALLY)
    s = re.sub(r'ly:hairpin::after-line-breaking',
                 r'ly:spanner::kill-zero-spanned-time', s)
    if re.search(r"(Slur|Tie)\w+#'dash-fraction", s) \
            or re.search(r"(Slur|Tie)\w+#'dash-period", s):
        stderr_write(NOT_SMART % "dash-fraction, dash-period")
        stderr_write(
            _("Dash parameters for slurs and ties are now in \'dash-definition.\n"))
        stderr_write(UPDATE_MANUALLY)
    return s


@rule((2, 13, 4), r"""Autobeaming rules have changed.  override-auto-beam-setting
and revert-auto-beam-setting have been eliminated.
\overrideBeamSettings has been added.
beatGrouping has been eliminated.
Different settings for vertical layout.
ly:system-start-text::print -> system-start-text::print
Beam #'thickness -> Beam #'beam-thickness
ly:note-head::brew-ez-stencil -> note-head::brew-ez-stencil
ly:ambitus::print -> ambitus::print
Explicit dynamics context definition from `Piano centered dynamics'
template replaced by new 'Dynamics' context""")
def conv(s):
    if re.search("override-auto-beam-setting", s):
        stderr_write(NOT_SMART % "override-auto-beam-setting")
        stderr_write(_(" \
   Autobeam settings are now overriden with \\overrideBeamSettings.\n"))
        stderr_write(UPDATE_MANUALLY)
    if re.search("revert-auto-beam-setting", s):
        stderr_write(NOT_SMART % "override-auto-beam-setting")
        stderr_write(_(" \
   Autobeam settings are now reverted with \\revertBeamSettings.\n"))
        stderr_write(UPDATE_MANUALLY)
    s = re.sub(r"\\set\s+beatGrouping", r"\\setBeatGrouping", s)
    if re.search(r"\w+\s*.\s*beatGrouping", s):
        stderr_write(NOT_SMART % "beatGrouping")
        stderr_write(_(" \
   beatGrouping with a specified context must now be accomplished with\n\
   \\overrideBeamSettings.\n"))
        stderr_write(UPDATE_MANUALLY)
    if re.search(r'alignment-offsets', s):
        stderr_write(NOT_SMART % "alignment-offsets")
        stderr_write(_("alignment-offsets has been changed to alignment-distances: \
you must now specify the distances between staves rather than the offset of staves.\n"))
        stderr_write(UPDATE_MANUALLY)
    s = re.sub('ly:(system-start-text::print|note-head::brew-ez-stencil|ambitus::print)',
                 '\\1', s)
    s = re.sub('(\\bBeam\\s+#\')(?=thickness\\b)', '\\1beam-', s)
    s = re.sub(r'(\\context\s*\{{1}[^\}]+\\type\s+"?Engraver_group"?\s+\\name\s+"*Dynamics"*[^\}]*\}{1})',
                 '% [Convert-ly] The Dynamics context is now included by default.', s)
    return s


@rule((2, 13, 10), """Remove obsolete engravers/translators: Note_swallow_translator,
Rest_swallow_translator, Skip_event_swallow_translator, Swallow_engraver,
Swallow_performer and String_number_engraver.
New vertical spacing variables""")
def conv(s):
    s = re.sub(r'\\(consists|remove)\s+"*(Swallow_(engraver|performer)|'
                 '(Note|Rest|Skip_event)_swallow_translator|String_number_engraver)"*',
                 '', s)

    # match through the end of assignments in the form "x = 30", "x = 1 \in", or "x = #3"
    s = re.sub(r"(page-top-space)\s*=\s*(([+-]?[.\d]*\s*\\[-\w]+)|(#?\s*[-+]?[.\d]+))",
                 r"obsolete-\g<0>"
                 r"  top-system-spacing #'space = #(/ obsolete-\1 staff-space)",
                 s)
    s = re.sub(r"(between-system-space)\s*=\s*(([+-]?[.\d]*\s*\\[-\w]+)|(#?\s*[-+]?[.\d]+))",
                 r"obsolete-\g<0>"
                 r"  between-system-spacing #'space = #(/ obsolete-\1 staff-space)"
                 r"  between-scores-system-spacing #'space = #(/ obsolete-\1 staff-space)",
                 s)
    s = re.sub(r"(between-system-padding)\s*=\s*(([+-]?[.\d]*\s*\\[-\w]+)|(#?\s*[-+]?[.\d]+))",
                 r"obsolete-\g<0>"
                 r"  between-system-spacing #'padding = #(/ obsolete-\1 staff-space)"
                 r"  between-scores-system-spacing #'padding = #(/ obsolete-\1 staff-space)",
                 s)
    s = re.sub(r"((before|between|after)-title-space)\s*=\s*(([+-]?[.\d]*\s*\\[-\w]+)|(#?\s*[-+]?[.\d]+))",
                 r"obsolete-\g<0>"
                 r"  \2-title-spacing #'space = #(/ obsolete-\1 staff-space)",
                 s)

    if re.search(r"VerticalAxisGroup\s*#\s*'minimum-Y-extent", s):
        stderr_write(NOT_SMART % "minimum-Y-extent")
        stderr_write(
            _("Vertical spacing no longer depends on the Y-extent of a VerticalAxisGroup.\n"))
        stderr_write(UPDATE_MANUALLY)

    return s


@rule((2, 13, 16), "Unify fetaNumber and fetaDynamic encodings")
def conv(s):
    return re.sub(r'\bfeta(Number|Dynamic)', 'fetaText', s)


@rule((2, 13, 18), r"\RemoveEmpty*StaffContext -> \*Staff \RemoveEmptyStaves")
def conv(s):
    s = re.sub(r"\\RemoveEmpty(|Drum|Rhythmic|Tab)StaffContext",
                 r"\\\1Staff \\RemoveEmptyStaves",
                 s)
    s = re.sub(r"\\AncientRemoveEmptyStaffContext",
                 r"\\VaticanaStaff \\RemoveEmptyStaves",
                 s)
    return s


@rule((2, 13, 20), r"\cresc etc. are now postfix operators")
def conv(s):
    s = re.sub(r'\\(cresc|dim|endcresc|enddim)\b', r'\\deprecated\1', s)
    return s


@rule((2, 13, 27), "interval-translate -> coord-translate")
def conv(s):
    s = re.sub('interval-translate', 'coord-translate', s)
    return s


@rule((2, 13, 29), r'''Eliminate beamSettings, beatLength, \setBeatGrouping,
\overrideBeamSettings and \revertBeamSettings.
"accordion.accEtcbase" -> "accordion.etcbass"''')
def conv(s):
    def sub_acc(m):
        d = {
            'Dot': 'dot',
            'Discant': 'discant',
            'Bayanbase': 'bayanbass',
            'Stdbase': 'stdbass',
            'Freebase': 'freebass',
            'OldEE': 'oldEE'
        }
        return '"accordion.%s"' % d[m.group(1)]

    s = re.sub(r'"accordion\.acc([a-zA-Z]+)"',
                 sub_acc, s)
    if re.search(r'overrideBeamSettings', s):
        stderr_write(NOT_SMART % "\\overrideBeamSettings")
        stderr_write(
            _("Use \\set beamExceptions or \\overrideTimeSignatureSettings.\n"))
        stderr_write(UPDATE_MANUALLY)
    if re.search(r'revertBeamSettings', s):
        stderr_write(NOT_SMART % "\\revertBeamSettings")
        stderr_write(
            _("Use \\set beamExceptions or \\revertTimeSignatureSettings.\n"))
        stderr_write(UPDATE_MANUALLY)
    if re.search(r'beamSettings', s):
        stderr_write(NOT_SMART % "beamSettings")
        stderr_write(_("Use baseMoment, beatStructure, and beamExceptions.\n"))
        stderr_write(UPDATE_MANUALLY)
    if re.search(r'beatLength', s):
        stderr_write(NOT_SMART % "beatLength")
        stderr_write(_("Use baseMoment and beatStructure.\n"))
        stderr_write(UPDATE_MANUALLY)
    if re.search(r'setBeatGrouping', s):
        stderr_write(NOT_SMART % "setbeatGrouping")
        stderr_write(_("Use baseMoment and beatStructure.\n"))
        stderr_write(UPDATE_MANUALLY)
    return s


@rule((2, 13, 31), """Woodwind diagrams: Move size, thickness, and graphic
from argument list to properties.
Deprecate negative dash-period for hidden lines: use #'style = #'none instead""")
def conv(s):
    if re.search(r'woodwind-diagram', s):
        stderr_write(NOT_SMART % "woodwind-diagrams")
        stderr_write(
            _("Move size, thickness, and graphic to properties.  Argument should be just the key list.\n"))
        stderr_write(UPDATE_MANUALLY)
    s = re.sub(r"dash-period\s+=\s*#\s*-[0-9.]+",
                 r"style = #'none",
                 s)
    return s


@rule((2, 13, 36), """Rename vertical spacing variables.
Add fretboard-table argument to savePredefinedFretboard""")
def conv(s):
    s = re.sub('after-title-spacing',           'markup-system-spacing', s)
    s = re.sub('before-title-spacing',          'score-markup-spacing',  s)
    s = re.sub('between-scores-system-spacing', 'score-system-spacing',  s)
    # this rule also converts page-breaking-between-system-spacing:
    s = re.sub('between-system-spacing',        'system-system-spacing', s)
    s = re.sub('between-title-spacing',         'markup-markup-spacing', s)
    s = re.sub('bottom-system-spacing',         'last-bottom-spacing',   s)
    s = re.sub('top-title-spacing',             'top-markup-spacing',    s)

    s = re.sub(r"storePredefinedDiagram",
                 r"storePredefinedDiagram #default-fret-table",
                 s)
    return s


@rule((2, 13, 39), "Rename vertical spacing grob properties")
def conv(s):
    # this rule also converts default-next-staff-spacing:
    s = re.sub('next-staff-spacing',
                 'staff-staff-spacing',             s)
    # this is not a mistake:
    #   Both 'next- and 'between- become 'staff-staff-spacing.
    #   There is no conflict since they are in different grobs.
    s = re.sub('between-staff-spacing',
                 'staff-staff-spacing',             s)
    s = re.sub('after-last-staff-spacing',
                 'staffgroup-staff-spacing',        s)
    s = re.sub('inter-staff-spacing',
                 'nonstaff-relatedstaff-spacing',   s)
    s = re.sub('non-affinity-spacing',
                 'nonstaff-unrelatedstaff-spacing', s)
    s = re.sub('inter-loose-line-spacing',
                 'nonstaff-nonstaff-spacing',       s)

    return s


@rule((2, 13, 40), r"Remove \paper variables head-separation and foot-separation")
def conv(s):
    if re.search(r'head-separation', s):
        stderr_write(NOT_SMART % "head-separation")
        stderr_write(_("Adjust settings for top-system-spacing instead.\n"))
        stderr_write(UPDATE_MANUALLY)
    if re.search(r'foot-separation', s):
        stderr_write(NOT_SMART % "foot-separation")
        stderr_write(_("Adjust settings for last-bottom-spacing instead.\n"))
        stderr_write(UPDATE_MANUALLY)

    return s


@rule((2, 13, 42), """Rename space to basic-distance in various spacing alists.
Remove HarmonicParenthesesItem grob""")
def conv(s):
    s = re.sub(
        r'\(space\s+\.\s+([0-9]*\.?[0-9]*)\)', r'(basic-distance . \1)', s)
    s = re.sub(r"#'space\s+=\s+#?([0-9]*\.?[0-9]*)",
                 r"#'basic-distance = #\1", s)
    if re.search(r'HarmonicParenthesesItem', s):
        stderr_write(NOT_SMART % "HarmonicParenthesesItem")
        stderr_write(_("HarmonicParenthesesItem has been eliminated.\n"))
        stderr_write(
            _("Harmonic parentheses are part of the TabNoteHead grob.\n"))
        stderr_write(UPDATE_MANUALLY)
    return s


@rule((2, 13, 44), "Remove context from overrideTimeSignatureSettings and revertTimeSignatureSettings")
def conv(s):
    s = re.sub(
        r"\\(override|revert)TimeSignatureSettings(\s+[^#]*)(#[^#]*)#", r"\\\1TimeSignatureSettings\2#", s)
    return s


@rule((2, 13, 46), """Change stringTunings from a list of semitones to a list of pitches.
Change tenor and baritone ukulele names in string tunings.
Generate messages for manual conversion of vertical spacing if required""")
def conv(s):
    def semitones2pitch(semitones):
        steps = [0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6]
        alterations = ["NATURAL", "SHARP", "NATURAL", "SHARP", "NATURAL",
                       "NATURAL", "SHARP", "NATURAL", "SHARP", "NATURAL", "SHARP", "NATURAL"]
        octave = 0
        while semitones > 11:
            octave += 1
            semitones -= 12
        while semitones < 0:
            octave -= 1
            semitones += 12
        pitchArgs = "%d %d %s" % (
            octave, steps[semitones], alterations[semitones])
        return pitchArgs

    def convert_tones(semitone_list):
        tones = semitone_list.split()
        res = ""
        for tone in tones:
            args = semitones2pitch(int(tone))
            res += ",(ly:make-pitch " + args + ") "
        return res

    def new_tunings(matchobj):
        return "stringTunings = #`(" + convert_tones(matchobj.group(1)) + ")"
    s = re.sub(r"stringTunings\s*=\s*#'\(([\d\s-]*)\)",
                 new_tunings, s)

    s = re.sub(r"ukulele-(tenor|baritone)-tuning", r"\1-ukulele-tuning", s)

    if re.search(r"[^-]page-top-space", s):
        stderr_write(NOT_SMART % "page-top-space")
        stderr_write(UPDATE_MANUALLY)
    if re.search(r"[^-]between-system-(space|padding)", s):
        stderr_write(NOT_SMART % "between-system-space, -padding")
        stderr_write(UPDATE_MANUALLY)
    if re.search(r"[^-](before|between|after)-title-space", s):
        stderr_write(NOT_SMART % "before-, between-, after-title-space")
        stderr_write(UPDATE_MANUALLY)
    if re.search(r"\\name\s", s):
        stderr_write(
            "\n" + _("Vertical spacing changes might affect user-defined contexts.") + "\n")
        stderr_write(UPDATE_MANUALLY)

    return s


@rule((2, 13, 48), "Replace bar-size with bar-extent")
def conv(s):
    def size_as_extent(matchobj):
        half = "%g" % (float(matchobj.group(1)) / 2)
        return "bar-extent = #'(-" + half + " . " + half + ")"

    s = re.sub(r"bar-size\s*=\s*#([0-9\.]+)", size_as_extent, s)

    return s


@rule((2, 13, 51), "Woodwind diagrams: Changes to the clarinet diagram")
def conv(s):
    if re.search(r'\\woodwind-diagram\s*#[^#]*clarinet\s', s):
        stderr_write(NOT_SMART % "woodwind-diagrams")
        stderr_write(
            _("Clarinet fingering changed to reflect actual anatomy of instrument.\n"))
        stderr_write(UPDATE_MANUALLY)
    return s


@rule((2, 14, 0), "bump version for release")
def conv(s):
    return s


@rule((2, 15, 7), "Handling of non-automatic footnotes")
def conv(s):
    if re.search(r'\\footnote', s):
        stderr_write(NOT_SMART % "\\footnote")
        stderr_write(
            _("If you are using non-automatic footnotes, make sure to set footnote-auto-numbering = ##f in the paper block.\n"))
        stderr_write(UPDATE_MANUALLY)
    return s


@rule((2, 15, 9), "Change in internal property for MultiMeasureRest")
def conv(s):
    if re.search(r'use-breve-rest', s):
        stderr_write(NOT_SMART % "use-breve-rest")
        stderr_write(
            _("This internal property has been replaced by round-up-to-longer-rest, round-up-exceptions and usable-duration-logs.\n"))
        stderr_write(UPDATE_MANUALLY)
    return s


@rule((2, 15, 10), "Creation of a Flag grob and moving of certain Stem properties to this grob")
def conv(s):
    s = re.sub(r"Stem\s+#'flag-style", r"Flag #'style", s)
    s = re.sub(r"Stem\s+#'stroke-style", r"Flag #'stroke-style", s)
    s = re.sub(r"Stem\s+#'flag", r"Flag #'stencil", s)
    s = re.sub(r"(\s+(?:\\once\s*)?)\\override\s+Stem\s+#'transparent\s*=\s*##t",
                 r"\g<1>\\override Stem #'transparent = ##t\g<1>\\override Flag #'transparent = ##t", s)
    s = re.sub(r"(\s+(?:\\once\s*)?)\\revert\s*Stem\s+#'transparent",
                 r"\g<1>\\revert Stem #'transparent\g<1>\\revert Flag #'transparent", s)
    s = re.sub(r"(\s+(?:\\once\s*)?)\\override\s+Stem\s+#'stencil\s*=\s*##f",
                 r"\g<1>\\override Stem #'stencil = ##f\g<1>\\override Flag #'stencil = ##f", s)
    s = re.sub(r"(\s+(?:\\once\s*)?)\\revert\s*Stem\s+#'stencil",
                 r"\g<1>\\revert Stem #'stencil\g<1>\\revert Flag #'stencil", s)
    return s


@rule((2, 15, 16), r"\makeStringTuning, \contextStringTuning -> \stringTuning")
def conv(s):
    s = re.sub(r"(\s+)\\contextStringTuning(\s+)#'([-a-zA-Z]+)(\s+<[^<>]+>)",
                 r"""\g<1>#(define \g<3> #{ \\stringTuning\g<4> #})\g<1>\\set stringTunings = #\g<3>""",
                 s)
    s = re.sub(r"""
\\makeStringTuning(\s+)#'([-a-zA-Z]+)""",
                 r"""
"\g<2>" = \\stringTuning""", s)
    s = re.sub(r"\\makeStringTuning(\s+)#'([-a-zA-Z]+)(\s+<[^<>]+>)",
                 r"#(define \g<2> #{ \\stringTuning\g<3> #})", s)
    return s


@rule((2, 15, 17), r"""\markuplines -> \markuplist
Change Beam broken slope syntax""")
def conv(s):
    s = re.sub(r"""
\\markuplines( +)([^ ].*)
            \1([^ ])""", r"""
\\markuplist\g<1>\g<2>
           \g<1>\g<3>""", s)
    s = re.sub(r"\\markuplines", r"\\markuplist", s)
    s = re.sub(r"@funindex markuplines", r"@funindex markuplist", s)
    if re.search(r'consistent-broken-slope', s):
        stderr_write(NOT_SMART % "consistent-broken-slope")
        stderr_write(
            _("consistent-broken-slope is now handled through the positions callback.\n"))
        stderr_write(
            _("input/regression/beam-broken-classic.ly shows how broken beams are now handled.\n"))
        stderr_write(UPDATE_MANUALLY)
    return s


def paren_matcher(n):
    # poor man's matched paren scanning, gives up
    # after n+1 levels.  Matches any string with balanced
    # parens inside; add the outer parens yourself if needed.
    # Nongreedy.
    return r"[^()]*?(?:\("*n+r"[^()]*?"+r"\)[^()]*?)*?"*n


def undollar_scm(m):
    return re.sub(r"\$(.?)", r"\1", m.group(0))


def undollar_embedded(m):
    s = re.sub(r"#\$", "#", m.group(1))
    # poor man's matched paren scanning after #, gives up
    # after 25 levels.
    s = re.sub(r"#`?\("+paren_matcher(25)+r"\)", undollar_scm, s)
    return m.string[m.start(0):m.start(1)] + s + m.string[m.end(1):m.end(0)]


def strip_export(s):
    return re.sub(r"\(ly:export\s+(" + paren_matcher(25) + r")\)",
                  r"\1", s)


def export_puller(m):
    if not re.search(r"ly:export\s+", m.group(0)):
        return m.group(0)
    return "$" + strip_export(m.string[m.start(0)+1:m.end(0)])


def ugly_function_rewriter(m):
    return m.string[m.start(0):m.start(1)] + strip_export(m.group(1)) + m.string[m.end(1):m.end(0)]


should_really_be_music_function = "(?:\
set-time-signature|empty-music|add-grace-property|\
remove-grace-property|set-accidental-style)"


def record_ugly(m):
    global should_really_be_music_function
    if not re.match(should_really_be_music_function, m.group(1)) \
            and re.search(r"ly:export\s+", m.group(2)):
        should_really_be_music_function = \
            should_really_be_music_function[:-1] + "|" + m.group(1) + ")"
    return m.group(0)


@rule((2, 15, 18), "#$ -> #, ly:export -> $")
def conv(s):
    s = re.sub(r"(?s)#@?\{(.*?)#@?\}", undollar_embedded, s)
    s = re.sub(r"#\(define(?:-public)?\s+\(([-a-zA-Z]+)"
                 + r"\b[^()]*?\)(" + paren_matcher(25)
                 + r")\)", record_ugly, s)
    s = re.sub(r"\(define(?:-public)?\s+\(" + should_really_be_music_function
                 + r"\b[^()]*\)(" + paren_matcher(25)
                 + r")\)", ugly_function_rewriter, s)
    s = re.sub(r"#(?=\(" + should_really_be_music_function + ")", "$", s)
    s = re.sub(r"#\(markup\*(?=\s)", r"$(markup", s)
    s = re.sub(r"#\("+paren_matcher(25)+r"\)", export_puller, s)
    if re.search(r"\(ly:export\s+", s):
        stderr_write(NOT_SMART % "ly:export")
    return s


@rule((2, 15, 19), r"$(set-time-signature ...) -> \time")
def conv(s):
    s = re.sub(r"\$\(set-time-signature\s+([0-9]+)\s+([0-9]+)\s*\)",
                 r"\\time \1/\2", s)
    s = re.sub(r"\$\(set-time-signature\s+([0-9]+)\s+([0-9]+)\s+(" +
                 paren_matcher(5) + r")\)", r"\\time #\3 \1/\2", s)
    if re.search(r"\(set-time-signature\s+", s):
        stderr_write(NOT_SMART % "set-time-signature")
    return s


@rule((2, 15, 20), r"$(set-accidental-style ...) -> \accidentalStyle")
def conv(s):
    s = re.sub(r"\$\(set-accidental-style\s+'([-a-z]+)\)",
                 r'\\accidentalStyle "\1"', s)
    s = re.sub(r"\$\(set-accidental-style\s+'([-a-z]+)\s+'([-A-Za-z]+)\s*\)",
                 r'''\\accidentalStyle #'\2 "\1"''', s)
    s = re.sub(r"(@funindex\s+)set-accidental-style",
                 r"\1\\accidentalStyle", s)
    return s

matchstring = r'"(?:[^"\\]|\\.)*"'
matcharg = (r"\s+(?:[$#]['`]?\s*(?:[a-zA-Z][^ \t\n()\\]*|" + matchstring
            + r"|#?\(" + paren_matcher(20) + r"\)|"
            + r"-?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)|"
            + r"#(?:[tf]|\\.|@?\{" + lilylib.brace_matcher(10) + r"#@?\}))|"
            + matchstring + r"|\\[a-z_A-Z]+|[0-9]+(?:/[0-9]+)?|-[0-9]+)")
matchfullmarkup = (r'\\markup\s*(?:@?\{' + lilylib.brace_matcher(20) + r'\}|' +
                   matchstring + r'|(?:\\[a-z_A-Z][a-z_A-Z-]*(?:' + matcharg +
                   r')*?\s*)*(?:' + matchstring + r"|@?\{" + lilylib.brace_matcher(20) +
                   r"\}))")
matchmarkup = "(?:" + matchstring + "|" + matchfullmarkup + ")"


@rule((2, 15, 25), r"\(auto)?Footnote(Grob)? -> \footnote")
def conv(s):
    # The following replacement includes the final markup argument in
    # the match in order to better avoid touching the equally named
    # markup function.  The other functions have unique names, so
    # there is no point in including their last, possibly complex
    # argument in the match.
    s = re.sub(r"\\footnote(" + matcharg + (r")(\s*" + matchmarkup)*2 + ")",
                 r"\\footnote\2\1\3", s)
    s = re.sub(r"\\footnoteGrob"+("(" + matcharg + ")")*2 + r"(\s*" + matchmarkup + ")",
                 r"\\footnote\3\2\1", s)
    s = re.sub(r"\\autoFootnoteGrob" + ("(" + matcharg + ")")*2,
                 r"\\footnote\2\1", s)
    s = re.sub(r"\\autoFootnote",
                 r"\\footnote", s)
    return s


@rule((2, 15, 32), r"tempoWholesPerMinute -> \tempo")
def conv(s):
    def sub_tempo(m):
        num = int(m.group(1))
        den = int(m.group(2))

        if (den & (den - 1)) != 0:
            return m.group(0)

        # Don't try dotted forms if they result in less than 30 bpm.
        # It is not actually relevant to get this right since this
        # only occurs in non-printing situations
        if den >= 16 and (num % 7) == 0 and num >= 210:
            return r"\tempo %d.. = %d" % (den/4, num/7)

        if den >= 8 and (num % 3) == 0 and num >= 90:
            return r"\tempo %d. = %d" % (den/2, num/3)

        return r"\tempo %d = %d" % (den, num)

    s = re.sub(r"\\context\s*@?\{\s*\\Score\s+tempoWholesPerMinute\s*=\s*" +
                 r"#\(ly:make-moment\s+([0-9]+)\s+([0-9]+)\)\s*@?\}",
                 sub_tempo, s)
    return s


@rule((2, 15, 39), r"\footnote ... -> \footnote ... \default")
def conv(s):
    def not_first(s):
        def match_fun(m):
            if m.group(1):
                return m.group(0)
            return m.expand(s)
        return match_fun
    s = re.sub("(" + matchfullmarkup + ")|"
                 + r"(\\footnote(?:\s*"
                 + matchmarkup + ")?" + matcharg + "(?:" + matcharg
                 + r")?\s+" + matchmarkup + ")",
                 not_first(r"\2 \\default"), s)
    return s


@rule((2, 15, 40), r"Remove beamWholeMeasure")
def conv(s):
    if re.search(r"\bbeamWholeMeasure\b", s):
        stderr_write(NOT_SMART % "beamWholeMeasure")
        stderr_write(
            _("beamExceptions controls whole-measure beaming.") + "\n")
    return s


@rule((2, 15, 42), r"\set stringTuning -> \set Staff.stringTuning")
def conv(s):
    s = re.sub(r"(\\set\s+)stringTuning", r"\1Staff.stringTuning", s)
    return s


wordsyntax = r"[a-zA-Z\200-\377]+(?:[-_][a-zA-Z\200-\377]+)*"


@rule((2, 15, 43), r'"custom-tuning" = -> custom-tuning =')
def conv(s):
    s = re.sub(
        '\n"(' + wordsyntax + r')"(\s*=\s*\\stringTuning)', "\n\\1\\2", s)
    return s


@rule((2, 16, 0), "bump version for release")
def conv(s):
    return s


@rule((2, 17, 0), r"blank-*-force -> blank-*-penalty")
def conv(s):
    s = re.sub('blank-page-force', 'blank-page-penalty', s)
    s = re.sub('blank-last-page-force', 'blank-last-page-penalty', s)
    s = re.sub('blank-after-score-page-force',
                 'blank-after-score-page-penalty', s)
    return s


@rule((2, 17, 4), r"\shape Grob #offsets -> \shape #offsets Grob")
def conv(s):
    s = re.sub(r"\\shape(\s+(?:[a-zA-Z]+|" + matchstring + "))(" +
                 matcharg + ")", r"\\shape\2\1", s)
    return s


barstring = r'''(?x)
( \\bar |
  whichBar |
  defaultBarType |
  segnoType |
  doubleRepeatType |
  startRepeatType |
  endRepeatType |
  doubleRepeatSegnoType |
  startRepeatSegnoType |
  endRepeatSegnoType |
  BarLine \s* [#]'glyph |
  BarLine \s* [#]'glyph-name |
  SpanBar \s* [#]'glyph-name )
( \s* [=]? \s* [#]? )
'''

@rule((2, 17, 5), r"New bar line interface")
def conv(s):
    s = re.sub(barstring + r'"\|:"', '\\1\\2".|:"', s)
    s = re.sub(barstring + r'":\|"', '\\1\\2":|."', s)
    s = re.sub(barstring + r'"\|\|:"', '\\1\\2".|:-||"', s)
    s = re.sub(barstring + r'":\|:"', '\\1\\2":..:"', s)
    s = re.sub(barstring + r'"\.\|\."', '\\1\\2".."', s)
    s = re.sub(barstring + r'"\|S"', '\\1\\2"S-|"', s)
    s = re.sub(barstring + r'"S\|"', '\\1\\2"S-S"', s)
    s = re.sub(barstring + r'":\|S"', '\\1\\2":|.S"', s)
    s = re.sub(barstring + r'":\|S\."', '\\1\\2":|.S-S"', s)
    s = re.sub(barstring + r'"S\|:"', '\\1\\2"S.|:-S"', s)
    s = re.sub(barstring + r'"\.S\|:"', '\\1\\2"S.|:"', s)
    s = re.sub(barstring + r'":\|S\|:"', '\\1\\2":|.S.|:"', s)
    s = re.sub(barstring + r'":\|S\.\|:"', '\\1\\2":|.S.|:-S"', s)
    s = re.sub(barstring + r'":"', '\\1\\2";"', s)
    s = re.sub(barstring + r'"\|s"', '\\1\\2"|-s"', s)
    s = re.sub(barstring + r'"dashed"', '\\1\\2"!"', s)
    s = re.sub(barstring + r'"kievan"', '\\1\\2"k"', s)
    s = re.sub(barstring + r'"empty"', '\\1\\2"-"', s)
    return s


symbol_list = (r"#'(?:" + wordsyntax + r"|\(\s*" + wordsyntax
               + r"(?:\s+" + wordsyntax + r")*\s*\))")

grob_path = symbol_list + r"(?:\s+" + symbol_list + r")*"

grob_spec = wordsyntax + r"(?:\s*\.\s*" + wordsyntax + r")?"

def path_replace(m):
    return m.group(1) + ".".join(re.findall(wordsyntax, m.group(2)))

def convert_overrides_to_dots(s):
    return re.sub(r"(\\(?:override|revert)\s+)(" + grob_spec + r"\s+" + grob_path + ")",
                  path_replace, s)

# The following regexp appears to be unusually expensive to compile,
# so we do it only once instead of for every file
footnotec = re.compile("(" + matchfullmarkup + ")|"
                       + r"(\\footnote(?:\s*"
                       + matchmarkup + ")?" + matcharg + ")(" + matcharg
                       + r")?(\s+" + matchmarkup + r")(\s+\\default)?")

@rule((2, 17, 6), r"""\accidentalStyle #'Context "style" -> \accidentalStyle Context.style
\alterBroken "Context.grob" -> \alterBroken Context.grob
\overrideProperty "Context.grob" -> \overrideProperty Context.grob
\tweak Grob #'symbol -> \tweak Grob.symbol""")
def conv(s):
    def patrep(m):
        def fn_path_replace(m):
            x = ".".join(re.findall(wordsyntax, m.group(2)))
            if x in ["TimeSignature", "KeySignature", "BarLine",
                     "Clef", "StaffSymbol", "OttavaBracket",
                     "LedgerLineSpanner"]:
                x = "Staff." + x
            return m.group(1) + x
        if m.group(1):
            return m.group(0)
        x = m.group(2) + m.group(4)

        if m.group(3):
            x = x + re.sub(r"(\s*)(" + symbol_list + ")", fn_path_replace,
                           m.group(3))

            if not m.group(5):
                x = r"\single" + x
        return x

    s = re.sub(r'''(\\accidentalStyle\s+)#?"([-A-Za-z]+)"''',
                 r"\1\2", s)
    s = re.sub(r'''(\\accidentalStyle\s+)#'([A-Za-z]+)\s+#?"?([-A-Za-z]+)"?''',
                 r"\1\2.\3", s)
    s = re.sub(r'''(\\(?:alterBroken|overrideProperty)\s+)#?"([A-Za-z]+)\s*\.\s*([A-Za-z]+)"''',
                 r"\1\2.\3", s)
    s = re.sub(r'''(\\tweak\s+)#?"?([A-W][A-Za-z]*)"?\s+?#'([a-zX-Z][-A-Za-z]*)''',
                 r"\1\2.\3", s)
    s = re.sub(r'''(\\tweak\s+)#'([a-zX-Z][-A-Za-z]*)''',
                 r"\1\2", s)
    s = footnotec.sub(patrep, s)
    s = re.sub(r'''(\\alterBroken)(\s+[A-Za-z.]+)(''' + matcharg
                 + matcharg + ")", r"\1\3\2", s)
    s = re.sub(r"(\\overrideProperty\s+)(" + grob_spec + r"\s+" + grob_path + ")",
                 path_replace, s)
    s = convert_overrides_to_dots(s)
    return s


@rule((2, 17, 11), r"""\times -> \tuplet, \set tupletSpannerDuration -> \tupletSpan
(ly:make-moment 1 4) -> (ly:make-moment 1/4)
(ly:make-duration 0 0 1 2) -> (ly:make-duration 0 0 1/2)""")
def conv(s):
    def sub_dur(m):
        num = int(m.group(1))
        den = int(m.group(2))

        # if den is no power of 2, don't even try to use an unscaled duration
        if (den & (den - 1)) != 0:
            return r"\tupletSpan 1*%d/%d" % (num, den)

        if den >= 4 and num == 7:
            return r"\tupletSpan %d.." % (den/4)

        if den >= 2 and num == 3:
            return r"\tupletSpan %d." % (den/2)

        if num == 1:
            return r"\tupletSpan %d" % den

        return r"\tupletSpan 1*%d/%d" % (num, den)

    s = re.sub(r"\\set\s+tupletSpannerDuration\s*=\s*" +
                 r"#\(ly:make-moment\s+([0-9]+)\s+([0-9]+)\s*\)",
                 sub_dur, s)
    s = re.sub(r"\\unset tupletSpannerDuration",
                 r"\\tupletSpan \\default", s)
    s = re.sub(r"\\times(\s*)([0-9]+)/([0-9]+)",
                 r"\\tuplet\1\3/\2", s)

    s = re.sub(r"(\(ly:make-moment\s+-?[0-9]+)\s+([1-9][0-9]*\))",
                 r"\1/\2", s)
    s = re.sub(r"(\(ly:make-moment\s+-?[0-9]+)\s+([0-9]+\s+-?[0-9]+)\s([0-9]+\))",
                 r"\1/\2/\3", s)
    s = re.sub(r"(\(ly:make-duration\s+-?[0-9]+\s+[0-9]+\s+[0-9]+)\s+([0-9]+\))",
                 r"\1/\2", s)
    return s


@rule((2, 17, 14), r"\accepts ... -> \accepts ... \defaultchild ...")
def conv(s):
    def matchaccepts(m):
        # First weed out definitions starting from an existing
        # definition: we assume that the inherited \defaultchild is
        # good enough for our purposes.  Heuristic: starts with a
        # backslash and an uppercase letter.
        if re.match(r"\s*\\[A-Z]", m.group(1)):
            return m.group(0)
        # existing defaultchild obviously trumps all
        if re.search(r"\\defaultchild[^-_a-zA-Z]", m.group(1)):
            return m.group(0)
        # take the first \\accepts if any and replicate it
        return re.sub("(\r?\n[ \t]*|[ \t]+)"
                      + r"""\\accepts(\s+(?:#?".*?"|[-_a-zA-Z]+))""",
                      r"\g<0>\1\\defaultchild\2",
                      m.group(0), 1)

    s = re.sub(r"\\context\s*@?\{(" + lilylib.brace_matcher(20) + r")\}",
                 matchaccepts, s)
    return s


@rule((2, 17, 15), r"""#(ly:set-option 'old-relative)
\relative -> \relative c'""")
def conv(s):
    if re.search(r"[#$]\(ly:set-option\s+'old-relative", s):
        stderr_write(NOT_SMART % "#(ly:set-option 'old-relative)")
        stderr_write(UPDATE_MANUALLY)
        raise FatalConversionError()
    # If the file contains a language switch to a language where the
    # name of c is not "c", we can't reliably know which parts of the
    # file will need "c" and which need "do".
    m = re.search(
        r'\\language\s(?!\s*#?"(?:nederlands|deutsch|english|norsk|suomi|svenska))"', s)
    if m:
        # Heuristic: if there is a non-commented { before the language
        # selection, we can't be sure.
        # Also if there is any selection of a non-do language.
        if (re.search("^[^%\n]*\\{", m.string[:m.start()], re.M)
                or re.search(r'\\language\s(?!\s*#?"(?:catalan|espanol|español|italiano|français|portugues|vlaams))"', s)):
            do = "$(ly:make-pitch 0 0)"
        else:
            do = "do'"
    else:
        do = "c'"
    s = re.sub(r"(\\relative)(\s+(\{|[\\<]))",
                 r"\1 " + do + r"\2", s)
    return s


@rule((2, 17, 18), "Rename OctavateEight to ClefModifier, rename related properties.")
def conv(s):
    s = re.sub('OctavateEight',
                 'ClefModifier',                   s)
    s = re.sub('octavate-eight-interface',
                 'clef-modifier-interface',        s)
    s = re.sub('clefOctavation',
                 'clefTransposition',              s)
    s = re.sub('clefOctavationFormatter',
                 'clefTranspositionFormatter',     s)
    s = re.sub('clefOctavationStyle',
                 'clefTranspositionStyle',         s)
    s = re.sub('cueClefOctavation',
                 'cueClefTransposition',           s)
    s = re.sub('cueClefOctavationFormatter',
                 'cueClefTranspositionFormatter',  s)
    s = re.sub('cueClefOctavationStyle',
                 'cueClefTranspositionStyle',      s)
    return s


@rule((2, 17, 19), r"\column { \vspace #2 } -> \column { \combine \null \vspace #2 }")
def conv(s):
    def vspace_replace(m):

        # vspace now always adds space and does not, for example, change the
        # impact of either baselineskip or descenders on the line above.
        #
        # We can't simulate the old behavior in a simpler manner.  A command
        # of its own is not really warranted since this behavior combines
        # badly enough with other spacing considerations (like baselineskip
        # and descenders) as to make it not all that useful.  So this
        # conversion rule is here more for compatibility's sake rather than
        # preserving desirable behavior.

        s = re.sub(r"(\\\\?)vspace(\s)",
                     r"\1combine \1null \1vspace\2", m.group(0))
        return s

    s = re.sub(r"\\(?:left-|right-|center-|)column\s*\{" + lilylib.brace_matcher(20) + r"\}",
                 vspace_replace, s)
    return s


@rule((2, 17, 20), r"Flag.transparent and Flag.color inherit from Stem")
def conv(s):
    s = re.sub(r"(((?:\\once\s*)?)\\override\s+((?:\w+\.)?)Stem\.(transparent|color)\s*=\s*(#\S+))\s+\2\\override\s+\3Flag\.\4\s*=\s*\5",
                 r"\1", s)
    s = re.sub(r"(((?:\\once\s*)?)\\revert\s+((?:\w+\.)?)Stem\.(transparent|color))\s+\2\\revert\s+\3Flag\.\4",
                 r"\1", s)
    s = re.sub(r"(\\tweak\s+((?:\w+\.)?)Stem\.(transparent|color)\s+(#\S+))\s+\\tweak\s+\2Flag\.\3\s+\4",
                 r"\1", s)
    return s


@rule((2, 17, 25), r'''\tempo 4. = 50~60 -> \tempo 4. = 50-60
-| -> -!
pipeSymbol, escapedParenthesisOpenSymbol ... -> "|", "\\(" ...''')
def conv(s):
    #  This goes for \tempo commands ending with a range, like
    #  = 50 ~ 60
    #  and uses - instead.  We don't explicitly look for \tempo since the
    #  complete syntax has a large number of variants, and this is quite
    #  unlikely to occur in other contexts
    s = re.sub(r"(=\s*[0-9]+\s*)~(\s*[0-9]+\s)", r"\1-\2", s)

    # Match strings, and articulation shorthands that end in -^_
    # so that we leave alone -| in quoted strings and c4--|
    def subnonstring(m):
        if m.group(1):
            return m.group(1)+"!"
        return m.group(0)
    s = re.sub(r"([-^_])\||" + matchstring +
                 r"|[-^_][-^_]", subnonstring, s)
    s = re.sub(r"\bdashBar\b", "dashBang", s)
    orig = ["pipeSymbol",
            "bracketOpenSymbol",
            "bracketCloseSymbol",
            "tildeSymbol",
            "parenthesisOpenSymbol",
            "parenthesisCloseSymbol",
            "escapedExclamationSymbol",
            "escapedParenthesisOpenSymbol",
            "escapedParenthesisCloseSymbol",
            "escapedBiggerSymbol",
            "escapedSmallerSymbol"]
    repl = [r'"|"',
            r'"["',
            r'"]"',
            r'"~"',
            r'"("',
            r'")"',
            r'"\\!"',
            r'"\\("',
            r'"\\)"',
            r'"\\>"',
            r'"\\<"']
    words = r"\b(?:(" + ")|(".join(orig) + r"))\b"

    def wordreplace(m):
        def instring(m):
            return re.sub(r'["\\]', r'\\\g<0>', repl[m.lastindex-1])
        if m.lastindex:
            return repl[m.lastindex-1]
        return '"' + re.sub(words, instring, m.group(0)[1:-1]) + '"'
    s = re.sub(words + "|" + matchstring, wordreplace, s)
    return s


staff_padding_warning = _(r"""
staff-padding now controls the distance to the baseline, not the nearest point.
""")

@rule((2, 17, 27), r'''\stringTuning \notemode -> \stringTuning''')
def conv(s):
    s = re.sub(r"\\stringTuning\s*\\notemode(\s*)@?\{\s*(.*?)\s*@?\}",
                 r"\\stringTuning\1\2", s)
    if re.search(r'[^-\w]staff-padding[^-\w]', s):
        stderr_write(NOT_SMART % "staff-padding")
        stderr_write(staff_padding_warning);
    return s


@rule((2, 17, 29), r'''Dynamic_engraver -> New_dynamic_engraver+Dynamic_align_engraver
New_dynamic_engraver -> Dynamic_engraver''')
def conv(s):
    s = re.sub("(\r?\n?[ \t]*\\\\(?:consists|remove)\\s*)(\"?)Dynamic_engraver\\2",
                 r"\1\2New_dynamic_engraver\2\1\2Dynamic_align_engraver\2",
                 s)
    # Should we warn about any remaining Dynamic_engraver?  Possibly it
    # will do the job just fine.
    s = re.sub("New_dynamic_engraver", "Dynamic_engraver", s)
    return s


@rule((2, 17, 97), r'''(make-relative (a b) b ...) -> make-relative (a b) #{ a b #}...''')
def conv(s):
    s = re.sub(r"(\(make-relative\s+\(\s*(([A-Za-z][-_A-Za-z0-9]*)" +
                 r"(?:\s+[A-Za-z][-_A-Za-z0-9]*)*)\s*\)\s*)\3(?=\s)",
                 r"\1(make-event-chord (list \2))", s)
    s = re.sub(r"(\(make-relative\s+\(\s*([A-Za-z][-_A-Za-z0-9]*" +
                 r"(?:\s+([A-Za-z][-_A-Za-z0-9]*))+)\s*\)\s*)\3(?=\s)",
                 r"\1(make-sequential-music (list \2))", s)
    return s


@rule((2, 18, 0), "bump version for release")
def conv(s):
    return s


@rule((2, 19, 2), r"\lyricsto \new/\context/... -> \new/\context/... \lyricsto")
def conv(s):
    word = r'(?:#?"[^"]*"|\b' + wordsyntax + r'\b)'
    s = re.sub(r"(\\lyricsto\s*" + word + r"\s*)(\\(?:new|context)\s*" + word
                 + r"(?:\s*=\s*" + word + r")?\s*)",
                 r"\2\1", s)
    s = re.sub(r"(\\lyricsto\s*" + word + r"\s*)\\lyricmode\b\s*",
                 r"\1", s)
    s = re.sub(r"(\\lyricsto\s*" + word + r"\s*)\\lyrics\b\s*",
                 r"\\new Lyrics \1", s)
    s = re.sub(r'\\lyricmode\s*(\\lyricsto\b)', r"\1", s)
    return s


@rule((2, 19, 7), "keySignature -> keyAlterations")
def conv(s):
    s = re.sub(r'\bkeySignature\b', 'keyAlterations', s)
    s = re.sub(r'\blastKeySignature\b', 'lastKeyAlterations', s)
    s = re.sub(r'\blocalKeySignature\b', 'localAlterations', s)
    return s


@rule((2, 19, 11), "thin-kern -> segno-kern")
def conv(s):
    s = re.sub(r'\bthin-kern\b', 'segno-kern', s)
    return s


# before_id is written in a manner where it will only substantially
# (rather than as a lookbefore assertion) match material that could
# not be part of a previous id.  In that manner, one replacement does
# not inhibit an immediately adjacent replacement.

before_id = r'(?:^|(?<!\\)(?:\\\\)+|(?<=[^-_\\a-zA-Z])|(?<=[^a-zA-Z][-_]))'

# after_id is a pure lookbehind assertion so its match string is
# always empty

after_id = r'(?![a-zA-Z]|[-_][a-zA-Z])'


@rule((2, 19, 16), r"""implicitTimeSignatureVisibility -> initialTimeSignatureVisibility
csharp -> c-sharp
TimeSignature: style = #'() -> style = #'numbered""")
def conv(s):
    s = re.sub(r'\bimplicitTimeSignatureVisibility\b',
                 'initialTimeSignatureVisibility', s)
    s = re.sub('(' + before_id + r'[a-g])((?:sharp){1,2}|(?:flat){1,2})'
                 + after_id, r'\1-\2', s)
    s = re.sub(r"""\\override
                   (\s+)
                   ([a-zA-Z]+\.)?TimeSignature.style
                   (\s*)
                   =
                   (\s*)
                   \#'\(\)""",
               r"\\override\1\2TimeSignature.style\3=\4#'numbered",
               s,
               flags=re.VERBOSE)

    s = re.sub(r"""\\tweak
                   (\s+)
                   (TimeSignature\.)?style
                   (\s*)
                   \#'\(\)
                   (\s+)
                   \\time
                   """,
               r"\\tweak\1\2style\3#'numbered\4\\time",
               s,
               flags=re.VERBOSE)
    return s


@rule((2, 19, 22), """whiteout -> whiteout-box
(define-xxx-function (parser location ...) -> (define-xxx-function (...)
(xxx ... parser ...) -> (xxx ... ...)
ChordNameVoice -> ChordNames""")
def conv(s):
    # whiteout -> whiteout-box
    s = re.sub(r"\\whiteout(?![a-z_-])", r"\\whiteout-box", s)
    s = re.sub(r"\b\.whiteout(?![a-z_-])\b", r".whiteout-box", s)
    s = re.sub(r"#'whiteout(?![a-z_-])\b", r"#'whiteout-box", s)
    s = re.sub(r"\bstencil-whiteout\b", r"stencil-whiteout-box", s)

    # (define-xxx-function (parser location ...) -> (define-xxx-function (...)
    def topsubst(s):
        def subst(m):
            def subsub(m):
                s = (m.group(1)
                     + re.sub(r'(?<=\s|["\\()])' + m.group(2) + r'(?=\s|["\\()])',
                              r'(*location*)',
                              re.sub(r'(?<=\s|["\\()])parser(?=\s|["\\()])',
                                     r'(*parser*)', topsubst(m.group(3)))))
                return s
            return re.sub(r'(\([-a-z]+\s*\(+)parser\s+([-a-z]+)\s*((?:.|\n)*)$',
                      subsub, m.group(0))
        return re.sub(r'\(define-(?:music|event|scheme|void)-function(?=\s|["(])'
                      + paren_matcher(20) + r'\)', subst, s)
    s = topsubst(s)

    # (xxx ... parser ...) -> (xxx ... ...)
    def repl(m):
        return m.group(1) + inner(m.group(2))

    def inner(s):
        s = re.sub(r"(\((?:" +
                     r"ly:parser-lexer|" +
                     r"ly:parser-clone|" +
                     r"ly:parser-output-name|" +
                     r"ly:parser-error|" +
                     r"ly:parser-define!|" +
                     r"ly:parser-lookup|" +
                     r"ly:parser-has-error\?|" +
                     r"ly:parser-clear-error|" +
                     r"ly:parser-set-note-names|" +
                     r"ly:parser-include-string|" +
                     r"note-names-language|" +
                     r"display-lily-music|" +
                     r"music->lily-string|" +
                     r"note-name->lily-string|" +
                     r"value->lily-string|"
                     r"check-grob-path|" +
                     r"event-chord-wrap!|" +
                     r"collect-bookpart-for-book|" +
                     r"collect-scores-for-book|" +
                     r"collect-music-aux|" +
                     r"collect-book-music-for-book|" +
                     r"scorify-music|" +
                     r"collect-music-for-book|" +
                     r"collect-book-music-for-book|" +
                     r"toplevel-book-handler|" +
                     r"default-toplevel-book-handler|" +
                     r"print-book-with-defaults|" +
                     r"toplevel-music-handler|" +
                     r"toplevel-score-handler|" +
                     r"toplevel-text-handler|" +
                     r"toplevel-bookpart-handler|" +
                     r"book-music-handler|" +
                     r"context-mod-music-handler|" +
                     r"bookpart-music-handler|" +
                     r"output-def-music-handler|" +
                     r"print-book-with-defaults-as-systems|" +
                     r"add-score|" +
                     r"add-text|" +
                     r"add-music|" +
                     r"make-part-combine-music|" +
                     r"make-directed-part-combine-music|" +
                     r"add-quotable|" +
                     r"paper-variable|" +
                     r"make-autochange-music|" +
                     r"context-mod-from-music|" +
                     r"context-defs-from-music)" +
                     r'(?=\s|[()]))(' + paren_matcher(20) + ")"
                     r"(?:\s+parser(?=\s|[()])|\s*\(\*parser\*\))", repl, s)
        return s
    s = inner(s)
    # This is the simplest case, resulting from one music function
    # trying to call another one via Scheme.  The caller is supposed
    # to have its uses of parser/location converted to
    # (*parser*)/(*location*) already.  Other uses of
    # ly:music-function-extract are harder to convert but unlikely.
    s = re.sub(r'(\(\s*\(ly:music-function-extract\s+' + wordsyntax +
                 r'\s*\)\s+)\(\*parser\*\)\s*\(\*location\*\)', r'\1',
                 s)

    s = re.sub(r'ChordNameVoice', r'ChordNames', s)
    return s


@rule((2, 19, 24), r"""music-has-type -> music-is-of-type?
\applyOutput #'Context -> \applyOutput Context""")
def conv(s):
    s = re.sub(r'(?<=\s|["\\()])' + "music-has-type" + r'(?=\s|["\\()])',
                 "music-is-of-type?", s)
    s = re.sub(r"(\\applyOutput\s+)#'([a-zA-Z])", r"\1\2", s)
    return s


@rule((2, 19, 28), r"c:5.x, c:5^x, c:sus -> c:3.5.x, c:3.5^x, c:5")
def conv(s):
    s = re.sub(r":5([.^][1-9])", r":3.5\1", s)
    # row back for self-defeating forms
    s = re.sub(r":3\.5((?:\.[0-9]+)*\^(?:[0-9]+\.)*)3\.", r":5\1", s)
    s = re.sub(
        r":3\.5((?:\.[0-9]+)*\^?:[0-9]+(?:\.[0-9]+)*)\.3(?![.0-9])", r":5\1", s)
    s = re.sub(r":3\.5((?:\.[0-9]+)*)\^3(?=\s|\})", r":5\1", s)
    s = re.sub(r":sus(?=\s|\})", ":5", s)
    s = re.sub(r":1\.5(?=\s|[.^}])", r":5", s)
    return s


@rule((2, 19, 29), r"partcombine*Once -> \once \partcombine*")
def conv(s):
    s = re.sub(r"(\\partcombine(?:Apart|Chords|Unisono|SoloII?|Automatic))Once\b",
                 r"\\once \1", s)
    s = re.sub(r"(\\partcombineForce" + matcharg + r")\s*##f(\s)",
                 r"\1\2", s)
    s = re.sub(r"(\\partcombineForce" + matcharg + r")\s*##t(\s)",
                 r"\\once \1\2", s)
    return s


@rule((2, 19, 32), r"whiteout-box -> whiteout")
def conv(s):
    s = re.sub(r"\\whiteout-box(?![a-z_-])", r"\\whiteout", s)
    s = re.sub(r"\b\.whiteout-box(?![a-z_-])\b", r".whiteout", s)
    s = re.sub(r"#'whiteout-box(?![a-z_-])\b", r"#'whiteout", s)
    return s


@rule((2, 19, 39), r"...-spacing #'prop... = -> ...-spacing.prop... =")
def conv(s):
    s = re.sub(r"(\s)((?:markup-markup-spacing|markup-system-spacing"
                 r"|score-markup-spacing|last-bottom-spacing"
                 r"|score-system-spacing|system-system-spacing"
                 r"|top-markup-spacing|top-system-spacing)"
                 r"(?:\s+#\s*'\s*" + wordsyntax + r")+)(?=\s*=)", path_replace, s)
    return s


@rule((2, 19, 40), r"\time #'(2 3) ... -> \time 2,3 ...")
def conv(s):
    def repl(m):
        return m.group(1) + re.sub(r"\s+", ",", m.group(2))

    s = re.sub(r"(beatStructure\s*=\s*)#'\(([0-9]+(?:\s+[0-9]+)+)\)",
                 repl, s)

    s = re.sub(r"(\\time\s*)#'\(([0-9]+(?:\s+[0-9]+)+)\)", repl, s)

    def repl(m):
        subst = re.sub(r"\s+", ",", m.group(1))
        return subst + (4 + len(m.group(1)) - len(subst)) * " " + m.group(2)

    s = re.sub(r"#'\(([0-9]+(?:\s+[0-9]+)+)\)(\s+%\s*beatStructure)",
                 repl, s)
    return s


@rule((2, 19, 46), r"\context ... \modification -> \context ... \with \modification")
def conv(s):
    word = r'(?:#?"[^"]*"|\b' + wordsyntax + r'\b)'
    mods = "|".join(re.findall("\n(" + wordsyntax + r")\s*=\s*\\with(?:\s|\\|\{)", s)
                    + ['RemoveEmptyStaves', 'RemoveAllEmptyStaves'])
    s = re.sub(r"(\\(?:drums|figures|chords|lyrics|addlyrics|"
                 + r"(?:new|context)\s*" + word
                 + r"(?:\s*=\s*" + word + r")?)\s*)(\\(?:" + mods + "))",
                 r"\1\\with \2", s)
    return s


id_grob_property_warning = _(r"""
Previously the "id" grob property (string) was used for SVG output.
Now "output-attributes" (association list) is used instead.
""")

@rule((2, 19, 49), r"""id -> output-attributes.id or output-attributes
for \tweak, \override, \overrideProperty, and \revert""")
def conv(s):
    # path cannot start with '-' or '_' and matches zero or more path
    # units that each end in a dot
    path = r"(?:[a-zA-Z\200-\377](?:[-_]?[a-zA-Z\200-\377])*(?:\s*\.\s*))*"

    # Manual editing is needed when id is set to #(...) or \xxx
    manual_edits = r"(\\(?:tweak|override|overrideProperty)\s+" + \
        path + r")id(\s*=?\s*(?:\\|#\s*\())"
    automatic = r"(\\(?:tweak|override|overrideProperty|revert)\s+" + path + r")id"
    if re.search(manual_edits, s):
        stderr_write(NOT_SMART % "\"output-attributes\"")
        stderr_write(id_grob_property_warning)
        stderr_write(UPDATE_MANUALLY)

    # First, for manual editing cases we convert 'id' to 'output-attributes'
    # because Grob.output-attributes.id = #(lambda ... ) will not work.
    # Then for the rest we convert 'id' to 'output-attributes.id'
    s = re.sub(manual_edits, r"\1output-attributes\2", s)
    s = re.sub(automatic, r"\1output-attributes.id", s)
    return s


@rule((2, 20, 0), r'''\language "deutsch": beh -> heh''')
def conv(s):
    changes = re.findall(r'\\language\s*#?"([a-zçñ]+)"', s)
    if changes and (changes.count('deutsch') == len(changes)):
        s = re.sub(r'\bbeh\b', 'heh', s)
    return s


matchscmarg = (r'(?:[a-zA-Z_][-a-zA-Z_0-9]*|"(?:[^\\"]|\\.)*"|[-+]?[0-9.]+|\('
               + paren_matcher(10) + r"\))")

string_duration_re = (r'(?P<startquote>#?")'
                      r'(?P<dur>1|2|4|8|16|32|64|128|256|breve|longa|maxima)'
                      r'(?P<ws>\s*)'
                      r'(?P<dots>[.]*)'
                      r'(?P<endquote>")')

def to_ly_duration(match):
    # Take a match against string_duration_re, possibly embedded in a
    # larger regex.  Return the corresponding duration in LilyPond
    # syntax with braces, like "{4.}" or "{\longa}".
    if match.group("dur") in (r"breve", r"longa", r"maxima"):
        new_dur = '\\' + match.group("dur")
    else:
        new_dur = match.group("dur")
    return (match.group()[:match.start("startquote")-match.start()]
            + '{' + new_dur
            + match.group("dots") + '}'
            + match.group()[match.end("endquote")-match.start():])

def to_scm_duration(match):
    # Same as string_to_ly_duration, with result in Scheme syntax.
    # "4." => (ly:make-duration 2 1)
    # "longa" => (ly:make-duration -2 0)
    dur_log = {"1": 0, "2": 1, "4": 2, "8": 3, "16": 4,
               "32": 5, "64": 6, "128": 7, "256": 8,
               "breve": -1, "longa": -2, "maxima": -4}[match.group("dur")]
    dot_count = len(match.group("dots"))
    new = f"(ly:make-duration {dur_log} {dot_count})"
    return (match.group()[:match.start("startquote")-match.start()]
            + new
            + match.group()[match.end("endquote")-match.start():])

# \note #"4." => \note {4.}  in 2.21.0
# \rest #"4." => \rest {4.} in 2.23.1
def convert_string_to_duration_for_command(markup_command, s):
    s = re.sub(rf'\\{markup_command}\s*{string_duration_re}', to_ly_duration, s)
    s = re.sub(rf'#:{markup_command}\s+{string_duration_re}', to_scm_duration, s)
    return s

@rule((2, 21, 0), r"""\note #"4." -> \note {4.}
\markup-command #" -> \markup-command "
\partcombine* -> \partCombine, \autochange -> \autoChange
scripts.trilelement -> scripts.trillelement
\fermataMarkup -> \fermata
remove \\powerChords, deprecate banter-chord-names and jazz-chord-names
\compressFullBarRests  -> \compressEmptyMeasures
""")
def conv(s):
    s = convert_string_to_duration_for_command("note", s)
    s = re.sub(r"\(tuplet-number::(?:fraction-with-notes|non-default-fraction-with-notes|append-note-wrapper)\s" +
                 paren_matcher(20) + r"\)",
               lambda match: re.sub(string_duration_re, to_scm_duration, match.group()),
               s)
    s = re.sub(r'(\\(?:fret-diagram(?:-terse)?|harp-pedal|justify-string'
                 r'|lookup|musicglyph|postscript|simple|tied-lyric|verbatim-file'
                 r'|with-url|wordwrap-string'
                 r'|discant|freeBass|stdBass|stdBassIV|stdBassV|stdBassVI'
                 r')\s*)[#$](\\?")',
                 r'\1\2', s)
    s = re.sub(r"\\partcombine(Force|Up|Down|Chords|Apart|Unisono|SoloI|SoloII|Automatic|)\b",
                 r"\\partCombine\1", s)
    s = re.sub(r"\\autochange", r"\\autoChange", s)
    s = re.sub(r'\\powerChords', '', s)
    s = re.sub(r'"scripts\.trilelement"', r'"scripts.trillelement"', s)
    s = re.sub(r"\\fermataMarkup", r"\\fermata", s)
    s = re.sub(r"\\(compress|expand)FullBarRests", r"\\\1EmptyMeasures", s)
    if re.search(r"#(banter|jazz)-chordnames", s):
        stderr_write(NOT_SMART % _("alternative chord naming functions"))
        stderr_write(UPDATE_MANUALLY)
    return s


@rule((2, 21, 2), r'''\tocItem "string" -> \tocItem \markup "string"''')
def conv(s):
    s = re.sub(r'\\tocItem\s+"', r'\\tocItem \\markup "', s)
    return s


@rule((2, 22, 0), "bump version for release")
def conv(s):
    return s


multi_measure_rest_warning = _(r"""
Instead of (for example)
  \markup \override #'(multi-measure-rest . #t) \rest-by-number #0 #0
use
  \markup \multi-measure-rest-by-number #1

The argument of \multi-measure-rest-by-number is the number of measures
the multi-measure rest lasts.
""")

@rule((2, 23, 1), r"""
combine u/d variants of triangle, do, re, and ti noteheads
rename bar line "S" to "S-||"
rename bar line "S-|" to "S"
\rest "4." -> \rest {4.}
""")
def conv(s):
    s = re.sub(r'"noteheads\.[ud](1|2)(triangle|(?:do|re|ti)(?:Thin)?)"',
               r'"noteheads.s\1\2"', s)
    s = re.sub(r'\\bar(\s+)"S"', r'\\bar\1"S-||"', s)
    s = re.sub(r'\\bar(\s+)"S-\|"', r'\\bar\1"S"', s)
    s = re.sub(r'segnoType(\s+=\s+)#?"S"', r'segnoType\1"S-||"', s)
    s = re.sub(r'segnoType(\s+=\s+)#?"S-\|"', r'segnoType\1"S"', s)
    s = convert_string_to_duration_for_command("rest", s)
    # Be more general than \override #'(multi-measure-rest . #t),
    # there's also \override #'((something . else) (multi-measure-rest . #t))
    if "#'(multi-measure-rest . #t)" in s and r"\rest-by-number" in s:
        # Don't convert blindly since it may also be use of \rest-by-number
        # for a normal rest and \rest with \override #'(multi-measure-rest . #t)
        # somewhere else.
        stderr_write(NOT_SMART % r"\override #'(multi-measure-rest . #t) \rest-by-number")
        stderr_write(multi_measure_rest_warning)
        stderr_write(UPDATE_MANUALLY)
    return s


melody_engraver_warning = _(r"""
If you had

  \override Stem.neutral-direction = #DOWN

and

  \override Stem.neutral-direction #'()

to turn the use of the Melody_engraver off
and on, respectively, you should instead use

  \set suspendMelodyDecisions = ##t

and

  \set suspendMelodyDecisions = ##f

""")

@rule((2, 23, 2), r"""
warn about behavior change of Melody_engraver with Stem.neutral-direction
adapt module names to (lily) namespace
""")
def conv(s):
    # Detect changes to the Stem.neutral-direction property
    # in conjunction with the Melody_engraver. The string
    # "Stem.neutral-direction" is sufficient for the former,
    # since there is no situation where \tweak neutral-direction ...
    # would cause the property to end up on the Stem.
    #
    # Convert
    #   \consists Melody_engraver
    #   \override Stem.neutral-direction = #'()
    # to just
    #   \consists Melody_engraver
    #
    # Warn about other uses, it's too tricky to convert them
    # (e.g., \tweak Stem.color \tweak Stem.neutral-direction #DOWN ...
    # should become \once \set suspendMelodyDecisions = ##t \tweak Stem.color ...).
    neutral_dir = r'Stem\.neutral-direction'
    neutral_dir_override = r"\\override\s+{}\s+=\s+#'\(\)".format(neutral_dir)
    melody_engraver = r'\\consists\s+"?Melody_engraver"?'
    typical_usage = r'({})\s+{}'.format(melody_engraver, neutral_dir_override)
    s = re.sub(typical_usage, r"\1", s)
    if re.search(neutral_dir, s) and re.search('Melody_engraver', s):
        stderr_write(NOT_SMART % _("Stem.neutral-direction with Melody_engraver"))
        stderr_write(melody_engraver_warning)
        stderr_write(UPDATE_MANUALLY)

    s = re.sub(r'\(scm (accreg|display-lily|graphviz|guile-debugger|song|to-xml)\)',
               r'(lily \1)', s)
    return s


round_filled_polygon_warning = _(r"""
ly:round-filled-polygon was renamed to ly:round-polygon and now takes
an additional optional parameter specifying whether the polygon is filled.
The default value of the extroversion optional parameter was changed from
-1 to 0.
""")

@rule((2, 23, 3), r"""
glyph-name-alist -> alteration-glyph-name-alist
ly:round-filled-polygon -> ly:round-polygon
""")
def conv(s):
    # The negative lookbehind assertion is to avoid matching
    # standard-alteration-glyph-name-alist and similar.
    s = re.sub(r"(?<!-)glyph-name-alist", "alteration-glyph-name-alist", s)
    if "ly:round-filled-polygon" in s:
        stderr_write(NOT_SMART % "ly:round-filled-polygon")
        stderr_write(round_filled_polygon_warning)
        stderr_write(UPDATE_MANUALLY)
    return s


trill_pitch_group_re = r"""TrillPitchGroup\.
(
  font # recognize all font-something properties
  | color
  | extra-offset
  | layer
  | output-attributes
  | rotation
  | stencil
  | stencils
  | transparent
  | whiteout # also whiteout-style
)
"""

repl = r"TrillPitchParentheses.\1"

on_the_fly_replacements = (
    (r"[\\#$]print-page-number-check-first", r"\\if \\should-print-page-number"),
    (r"[\\#$]create-page-number-stencil", r"\\if \\should-print-page-numbers-global"),
    (r"[\\#$]print-all-headers", r"\\if \\should-print-all-headers"),
    (r"[\\#$]first-page", r"\\if \\on-first-page"),
    (r"[\\#$]not-first-page", r"\\unless \\on-first-page"),
    (r"[#$]\(on-page (\d+)\)", r"\\if \\on-page #\1"),
    (r"[\\#$]last-page", r"\\if \\on-last-page"),
    (r"[\\#$]part-first-page", r"\\if \\on-first-page-of-part"),
    (r"[\\#$]not-part-first-page", r"\\unless \\on-first-page-of-part"),
    (r"[\\#$]part-last-page", r"\\if \\on-last-page-of-part"),
    (r"[\\#$]not-single-page", r"\\unless \\single-page"),
)

@rule((2, 23, 4), r"""
ly:context-now -> ly:context-current-moment
ControlPointItem, ControlPointSpanner -> ControlPoint
ControlPolygonItem, ControlPolygonSpanner -> ControlPolygon
FootnoteItem, FootnoteSpanner -> Footnote
BalloonTextItem, BalloonTextSpanner -> BalloonText
\on-the-fly #some-procedure -> \if \some-condition
""")
def conv(s):
    s = re.sub("ly:context-now", "ly:context-current-moment", s)
    # It's unlikely that users would have wanted different settings
    # for the item type and the spanner type, so this should be reasonable.
    item_spanner = (r"(ControlPoint|ControlPolygon|Footnote|BalloonText)"
                    r"(Item|Spanner)")
    s = re.sub(item_spanner, r"\1", s)
    s = re.sub("ParenthesesItem", "Parentheses", s)
    s = re.sub("parentheses-item::", "parentheses-interface::", s)
    s = re.sub(trill_pitch_group_re, repl, s, flags=re.VERBOSE)
    for pattern, replacement in on_the_fly_replacements:
        complete_pattern = r"\\on-the-fly\s+" + pattern
        s = re.sub(complete_pattern, replacement, s)
    return s


mark_sequences_warning = _(r"""
If independent mark sequences are desired, use multiple Mark_tracking_translators.
""")

@rule((2, 23, 5), """
Mark_tracking_translator
""")
def conv(s):
    if re.search(r'\\consists\s+"?Mark_engraver"?', s):
        stderr_write(NOT_SMART % "\\consists Mark_engraver")
        stderr_write(mark_sequences_warning)
        stderr_write(UPDATE_MANUALLY)
    return s


dash_abbreviations = ["Hat", "Plus", "Dash", "Bang", "Larger", "Dot", "Underscore"]

markup2string_warning = _("""
The signature of the markup->string Scheme function changed.  Calls with
just one argument are not affected.  Calls using the second optional
argument, the list of header modules, should be changed from

  (markup->string <markup> <header list>)

to

  (markup->string <markup> #:props (headers-property-alist-chain <header list>))

""")

@rule((2, 23, 6), r"""
defaultBarType -> measureBarType
markFormatter -> rehearsalMarkFormatter
startRepeatType -> startRepeatBarType (etc.)
make-articulation "X" -> make-articulation 'X
'articulation-type "X" -> 'articulation-type 'X
dashX = "Y" -> dashX = #(make-articulation 'Y)
markup->string 2nd argument change
ly:grob-spanned-rank-interval -> ly:grob-spanned-column-rank-interval
""")
# It would be nicer to do
# dashX = "Y" -> dashX = \Y
# but it is not guaranteed that for any 'symbol valid as 'articulation-type
# (see the list in scm/script.scm), there is a corresponding after-event \symbol
# defined via #(make-articulation 'symbol ...) in ly/script-init.ly or
# ly/gregorian.ly. (Example: There is no \comma as of 2.23.5)
def conv(s):
    s = re.sub("defaultBarType", "measureBarType", s)
    s = re.sub("doubleRepeatSegnoType", "doubleRepeatSegnoBarType", s)
    s = re.sub("doubleRepeatType", "doubleRepeatBarType", s)
    s = re.sub("endRepeatSegnoType", "endRepeatSegnoBarType", s)
    s = re.sub("endRepeatType", "endRepeatBarType", s)
    s = re.sub("fineSegnoType", "fineSegnoBarType", s)
    s = re.sub("fineStartRepeatSegnoType", "fineStartRepeatSegnoBarType", s)
    s = re.sub("markFormatter", "rehearsalMarkFormatter", s)
    s = re.sub("segnoType", "segnoBarType", s)
    s = re.sub("startRepeatSegnoType", "startRepeatSegnoBarType", s)
    s = re.sub("startRepeatType", "startRepeatBarType", s)
    s = re.sub("underlyingRepeatType", "underlyingRepeatBarType", s)
    s = re.sub(r'''((make-articulation|'articulation-type)\s+)"(\w+)"''', r"\1'\3", s)
    s = re.sub(r'(dash(%s)\s+)=(\s+)"(\w+)"' % "|".join(dash_abbreviations),
               r"\1=\3#(make-articulation '\4)", s)
    # The case (markup->string <symbol>) is easy to detect and should
    # not be warned about.  Cases with one argument that is more complex
    # than a symbol are harder to detect reliably, so we conservatively
    # print the warning.
    if re.search(r"(?!(?<=\()markup\->string\s+\w+\))markup->string", s):
        stderr_write(NOT_SMART % "markup->string")
        stderr_write(markup2string_warning)
        stderr_write(UPDATE_MANUALLY)
    s = s.replace("ly:grob-spanned-rank-interval", "ly:grob-spanned-column-rank-interval")
    return s


bar_numbers_warning = _(r"""
Warning:

Your score contains a setting of barNumberVisibility to
#all-bar-numbers-visible and a setting of BarNumber.break-visibility,
but not \bar "" command.  This likely means that you have
been using

  \set Score.barNumberVisibility = #all-bar-numbers-visible
  \override Score.BarNumber.break-visibility = #end-of-line-invisible

in order to print bar numbers in the middle of systems
in addition to bar numbers at the beginning of systems.
In 2.23.7 and later, this will print the first bar number
too, which has always been the intended effect of
#all-bar-numbers-visible, but did not work without \bar ""
for technical reasons.  If you do not want the first
bar number, remove the command

  \set Score.barNumberVisibility = #all-bar-numbers-visible
""")

@rule((2, 23, 7), r"""
all-bar-numbers-visible + BarNumber.break-visibility + no \bar "" -> warning
""")
def conv(s):
    if (
        "all-bar-numbers-visible" in s
        and "BarNumber.break-visibility" in s
        and r'\bar ""' not in s
    ):
        stderr_write(bar_numbers_warning)
    return s


@rule((2, 23, 8), r"""
scripts.augmentum -> dots.dotvaticana
scripts.trillelement -> scripts.trill_element
ly:skyline::get-touching-point -> ly:skyline-touching-point
ly:skyline::get-distance -> ly:skyline-distance
ly:skyline::get-max-height -> ly:skyline-max-height
ly:skyline::get-max-height-position -> ly:skyline-max-height-position
ly:skyline::get-height -> ly:skyline-height
Remove Default_bar_line_engraver
""")
def conv(s):
    s = re.sub(r"scripts\.augmentum", r"dots.dotvaticana", s)
    s = re.sub(r"scripts\.trillelement", r"scripts.trill_element", s)
    s = re.sub(r"ly:skyline::get-touching-point", "ly:skyline-touching-point", s)
    s = re.sub(r"ly:skyline::get-distance", "ly:skyline-distance", s)
    s = re.sub(r"ly:skyline::get-max-height", "ly:skyline-max-height", s)
    s = re.sub(r"ly:skyline::get-max-height-position", "ly:skyline-max-height-position", s)
    s = re.sub(r"ly:skyline::get-height", "ly:skyline-height", s)
    s = re.sub(r'[\t ]*\\(consists|remove)\s*"?Default_bar_line_engraver"?[\t ]*\n?',
               r'', s)
    return s


percent_x_off_warning = _(r"""
The X-offset property of PercentRepeat is no longer relative to the
default position (centered), but to the left of the span within
which the object is centered.  Overrides/tweaks to PercentRepeat.X-offset
should be updated.
""")

@rule((2, 23, 9), r"""
ly:percent-repeat-item-interface::xxx -> ly:percent-repeat-interface::xxx
""")
def conv(s):
    s = re.sub(r"ly:percent-repeat-item-interface::", "ly:percent-repeat-interface::", s)
    if "PercentRepeat.X-offset" in s:
        stderr_write(NOT_SMART % "PercentRepeat.X-offset")
        stderr_write(percent_x_off_warning)
        stderr_write(UPDATE_MANUALLY)
    return s


automatic_bars_warning = _(r"""
The automaticBars property has been removed.  Instead, set
measureBarType to #'() or a valid bar type.
""")

@rule((2, 23, 10), r"""
automaticBars = ##f -> measureBarType = #'()
\bar "-" -> \bar ""
BarType = "-" -> BarType = ""
\featherDurations #(ly:make-moment x/y) -> \featherDurations x/y
\consists Chord_name_engraver -> \consists Chord_name_engraver
                                 \consists Current_chord_text_engraver
\remove Chord_name_engraver -> \remove Chord_name_engraver
                               \remove Current_chord_text_engraver
""")
def conv(s):
    s = re.sub(r"automaticBars\s*=\s*##f", r"measureBarType = #'()", s)
    if "automaticBars" in s:
        stderr_write(NOT_SMART % _("advanced use of automaticBars"))
        stderr_write(automatic_bars_warning)
        stderr_write(UPDATE_MANUALLY)
    s = re.sub(r'\\bar\s*"-"', r'\\bar ""', s)
    s = re.sub(r'BarType\s*=\s*"-"', r'BarType = ""', s)
    s = re.sub(r'(\\featherDurations\s+)#\(ly:make-moment\s+([0-9/]+)\)',
               r'\1\2', s)
    s = re.sub(r'\\(consists|remove) "?Chord_name_engraver"?',
               r'\\\1 Chord_name_engraver \\\1 Current_chord_text_engraver',
               s)
    # This change is also in the rule for 2.13.18.  It is being
    # replicated here because the removal was only done in 2.23.10.
    # This ensures that a score that had previously been updated to a
    # version between 2.13.18 and 2.23.10 without running convert-ly
    # can still be fixed in the 2.23.10 upgrade using convert-ly.
    # Since the change was advertised earlier, this is not part of the
    # rule description.
    s = re.sub(r"\\RemoveEmpty(|Drum|Rhythmic|Tab)StaffContext",
                 r"\\\1Staff \\RemoveEmptyStaves",
                 s)
    s = re.sub(r"\\AncientRemoveEmptyStaffContext",
                 r"\\VaticanaStaff \\RemoveEmptyStaves",
                 s)
    return s


@rule((2, 23, 11), r"""
\bar "B" -> \bar "B-|" for B in { .| .|: [|: S S.|: }
""")
def conv(s):
    # We convert only \bar because automatic bar lines configured with
    # context properties layer themselves.
    changed_bar_types = [ # bar types which had an implicit "|" at EOL
        r'\.\|',    # .|
        r'\.\|:',   # .|:
        r'\[\|:',   # [|:
        r'S',       # S
        r'S\.\|:']  # S.|:
    s = re.sub(r'\\bar\s*"(' + '|'.join(changed_bar_types) + ')"',
               r'\\bar "\1-|"', s)
    # New syntax was introduced in 2.17.6, but this version adds a warning,
    # which is more insistent.
    s = convert_overrides_to_dots(s)
    return s


fine_iteration_warning = _(r"""
Warning: \fine no longer enforces the end of the music.  If your piece
has music following \fine that you want to exclude when it is
unfolded, use \volta to exclude it.
""")

new_ancient_divisio_grob_warning = _(r"""
MensuralStaff and VaticanaStaff now use Divisio_engraver to engrave
divisiones as Divisio grobs.

Previously, the respective Voice-aliased contexts used
Breathing_sign_engraver to engrave divisiones as BreathingSign grobs.
Because of the new names and the move from Voice to Staff, layout
adjustments for the old scheme are not effective for the new.

If you are not content with the new default layout, deal with Divisio
and Divisio_engraver in the appropriate Staff-aliased context.
""")

new_modern_divisio_grob_warning = _(r"""
GregorianTranscriptionStaff now engraves \divisioMinima,
\divisioMaior, and \divisioMaxima as BarLine grobs using Bar_engraver,
and engraves \caesura and \virgula as Divisio grobs using
Divisio_engraver.

Previously, GregorianTranscriptionVoice used Breathing_sign_engraver
to engrave these as BreathingSign grobs.  Because of the new names and
the move from Voice to Staff, layout adjustments for the old scheme
are not effective for the new.

If you are not content with the new default layout, deal with BarLine,
Bar_engraver, Divisio, and Divisio_engraver in
GregorianTranscriptionStaff context.  \EnableGregorianDivisiones may
also be used to switch to engraving Divisio grobs instead of BarLine
grobs.
""")

@rule((2, 23, 12), r"""
barAlways = ##t -> forbidBreakBetweenBarLines = ##f
\fine no longer stops iteration
New Divisio grob
""")
def conv(s):
    s = re.sub(r'barAlways\s*=\s*##t', r'forbidBreakBetweenBarLines = ##f', s)
    s = re.sub(r'barAlways\s*=\s*##f', r'forbidBreakBetweenBarLines = ##t', s)
    if ('\\fine' in s) and (('\\repeat segno' in s) or
                            ('\\repeat volta' in s)):
        stderr_write(NOT_SMART % _(r"music following \fine"))
        stderr_write(fine_iteration_warning)
        stderr_write(UPDATE_MANUALLY)
    if re.search(r"GregorianTranscription(Staff|Voice)", s) and \
        (("BreathingSign" in s) or ("Breathing_sign_engraver" in s)):
         stderr_write(NOT_SMART % _("BreathingSign to BarLine or Divisio"))
         stderr_write(new_modern_divisio_grob_warning)
         stderr_write(UPDATE_MANUALLY)
    if re.search(r"(Mensural|Vaticana)(Staff|Voice)", s) and \
        (("BreathingSign" in s) or ("Breathing_sign_engraver" in s)):
         stderr_write(NOT_SMART % _("BreathingSign to Divisio"))
         stderr_write(new_ancient_divisio_grob_warning)
         stderr_write(UPDATE_MANUALLY)
    return s


remove_bar_always_warning = _(r"""
The barAlways property has been removed.  Instead, use
forbidBreakBetweenBarLines.
""")

@rule((2, 23, 13), r"""
filtered-map -> filter-map
Remove barAlways
""")
def conv(s):
    s = re.sub(r"filtered-map", "(@ (srfi srfi-1) filter-map)", s)
    if "barAlways" in s:
        stderr_write(NOT_SMART % _("advanced use of barAlways"))
        stderr_write(remove_bar_always_warning)
        stderr_write(UPDATE_MANUALLY)
    return s


skylines_for_stencil_warning = _(r"""
The second argument to ly:skylines-for-stencil is now the 'horizon axis',
which is the opposite of the convention used previously.
""")

@rule((2, 23, 14), r"""
changed convention for ly:skylines-for-stencil second argument
""")
def conv(s):
    if "ly:skylines-for-stencil" in s:
        stderr_write(NOT_SMART % _("ly:skylines-for-stencil second argument"))
        stderr_write(skylines_for_stencil_warning)
        stderr_write(UPDATE_MANUALLY)
    return s


@rule((2, 24, 0), "bump version for release")
def conv(s):
    return s


@rule((2, 25, 0), r"""
\override Staff.StaffSymbol.line-positions = #'() ->
  \override Staff.StaffSymbol.line-positions = #ly:staff-symbol::calc-line-positions
""")
def conv(s):
    s = re.sub(r"(\\override (\w+\.)?StaffSymbol\.line-positions\s*=\s*)#'\(\)",
               r"\1#ly:staff-symbol::calc-line-positions",
               s)
    return s


string_regexp_substitute_warning = _(r"""
The string-regexp-substitute function has been removed.  A call

  (string-regexp-substitute pattern replacement str)

should be replaced with

  (ly:regex-replace (ly:make-regex pattern) str replacement)
""")

uniqued_list_warning = _(r"""
uniqued-alist no longer needs to take '() as its second argument.
""")

@rule((2, 25, 1), r"""
string-regexp-substitute removal
(uniqued-alist x '()) -> (uniqued-alist x)
""")
def conv(s):
    if "string-regexp-substitute" in s:
        stderr_write(NOT_SMART % "string-regexp-substitute")
        stderr_write(string_regexp_substitute_warning)
        stderr_write(UPDATE_MANUALLY)
    if "uniqued-alist" in s:
        stderr_write(NOT_SMART % "uniqued-alist")
        stderr_write(uniqued_list_warning)
        stderr_write(UPDATE_MANUALLY)
    return s


@rule((2, 25, 2), r"""
Add `\language "arabic"` if `hel-arabic.ly` gets included.
""")
def conv(s):
    s = re.sub(r'(?xm) ^ ( \s* \\include \s+ "hel-arabic\.ly" .* )',
               r'\1\n\\language "arabic"',
               s)
    return s


make_moment_re = (r'(?P<start>#\(\s*ly:make-moment\s+)'
                  r'(?P<numerator>\d+)'
                  r'(?P<separator>(/|\s+))'
                  r'(?P<denominator>\d+)'
                  r'(?P<end>\s*\))')

def make_moment_to_music_length(match):
    n = int(match.group('numerator'))
    d = int(match.group('denominator'))
    # It's tempting to use musicexp.Duration, but we want this frozen in time.
    # We also avoid reducing fractions; for example, we turn `4/4` into `4*4`
    # rather than `1`.
    dlog = d.bit_length() - 1
    if (1 << dlog) == d: # d is a power of 2
        if n == 1:
            return f'\\musicLength {d}'
        elif n == 3: # e.g. 3/8 -> 4.
            return f'\\musicLength {(1 << (dlog - 1))}.'
        else:
            return f'\\musicLength {d}*{n}'
    return f'\\musicLength 1*{n}/{d}'

@rule((2, 25, 3), r"""
#(ly:make-moment) -> \musicLength
""")
def conv(s):
    # #(ly:make-moment num den) or #(ly:make-moment num/den)
    s = re.sub(make_moment_re, make_moment_to_music_length, s)
    # #(ly:make-moment n)
    s = re.sub(r'#\(\s*ly:make-moment\s+(\d+)\s*\)', r'\\musicLength 1*\1', s)
    # clean up 1*1 from previous rule
    s = re.sub(r'\\musicLength 1\*1(?!\S)', r'\\musicLength 1', s)
    return s


@rule((2, 25, 4), r"""
#(define fonts (set-global-fonts #:roman "roman" ...))
or
#(define fonts (make-pango-font-tree ...))

->
  fonts.roman = ...
  fonts.sans = ...
  (etc.)


\lookup "non-brace glyph" -> \musicglyph "non-brace glyph"

font-shape = #'caps  ->  font-variant = #'small-caps
""")
def conv(s):
    # This replacement is not 100% reliable, but should do a good job.
    # If there is the #:factor (/ staff-height pt 20) parameter in the
    # set-global-fonts call, the user had done what it took to heed any
    # custom font size, so we're fine. If no #:factor was passed, we
    # assume that the user was not changing the font size, as doing that
    # without passing #:factor was resulting in bad output anyway.
    # Other values result in a warning.
    set_global_fonts_factor_warning = _("""
LilyPond now uses a different syntax for selecting fonts.  The
set-global-fonts Scheme function has been removed.  convert-ly
has mostly converted the call for you, but it was not able to
convert the #:factor parameter.  Now, font selection is independent
from setting the font size.  Please use set-global-staff-size
or layout-set-staff-size to replace this parameter, which you
set to:

    {}

""")

    set_global_fonts_brace_warning = _(r"""
LilyPond now uses a different syntax for selecting fonts.  The #:brace
argument to set-global-fonts does not have an equivalent in the new syntax;
braces now use the normal music font family by default.  Your code contains
a call to set-global-fonts where the #:music and #:brace parameters have
different values.  If you wish to set braces in a different music font than
other music glyphs, use code such as

\paper {
  fonts.music-alt = "your-music-font"
}

\layout {
  \context {
    \Score
    \override SystemStartBrace.font-family = #'music-alt
  }
}
""")

    set_global_fonts_advanced_warning = _(r"""
LilyPond now uses a different syntax for selecting fonts. The code equivalent
to the previous spelling

\paper {
  #(define fonts
     (set-global-fonts #:music "music-font"
                       #:brace "brace-font"
                       #:roman "roman font"
                       #:sans "sans font"
                       #:typewriter "typewriter font"))
}

is now

\paper {
  fonts.music = "music-font"
  fonts.roman = "roman font"
  fonts.sans = "sans font"
  fonts.typewriter = "typewriter font"
}

convert-ly found an advanced use of set-global-fonts that it was not able to
convert automatically.  Please do the update manually.
""")
    simple_sexpr_re = r'("[^"]+"|[\w/-]+)'
    maybe_funcall_sexpr_re = (rf'(\(({lilylib.paren_matcher(10)})\)'
                              rf'|{simple_sexpr_re})')
    keyval_re = rf'#:(?P<key>\w+)\s+(?P<val>{maybe_funcall_sexpr_re})\s*'
    set_global_fonts_re = rf"(\(set-global-fonts\s+(?P<args>({keyval_re})*)\))"
    indent_re = r"^(?P<indent>[^\S\n]*)"

    def replace_set_global_fonts(match):
        indent = match.group("indent")
        call = match.group("args")
        lines = []
        params = {}
        for keyval in re.finditer(keyval_re, call):
            params[keyval.group("key")] = keyval.group("val")
        if "factor" in params:
            fac = params["factor"].split()
            if (
                fac != ["(/", "staff-height", "pt", "20)"]
                and fac != ["(/", "staff-height", "20", "pt)"]
            ):
                stderr_write(NOT_SMART % _("#:factor parameter to set-global-fonts"))
                stderr_write(set_global_fonts_factor_warning
                             .format(params["factor"]))
                stderr_write(UPDATE_MANUALLY)
        if params.get("music") != params.get("brace"):
            stderr_write(NOT_SMART % _("different music and brace fonts passed to set-global-fonts"))
            stderr_write(set_global_fonts_brace_warning)
            stderr_write(UPDATE_MANUALLY)
        for key, val in params.items():
            if key == "factor" or key == "brace":
                continue
            if not val.startswith('"'):
                val = "#" + val
            lines.append(indent + f"fonts.{key} = {val}")
        return "\n".join(lines)

    define_re = rf"{indent_re}#\(define\s+fonts\s+{set_global_fonts_re}\s*\)"
    assign_re = rf"{indent_re}fonts\s*=\s*#{set_global_fonts_re}"
    s = re.sub(define_re, replace_set_global_fonts, s, flags=re.MULTILINE)
    s = re.sub(assign_re, replace_set_global_fonts, s, flags=re.MULTILINE)
    # Warn about remaining uses
    if "set-global-fonts" in s:
        stderr_write(NOT_SMART % _("advanced use of set-global-fonts"))
        stderr_write(set_global_fonts_advanced_warning)
        stderr_write(UPDATE_MANUALLY)

    # Similar logic here with the factor parameter here
    pango_warning = _("""
LilyPond now uses a different syntax for selecting fonts.  The
make-pango-font-tree Scheme function has been removed.  convert-ly
has mostly converted the call for you, but it was not able to
convert the #:factor parameter.  Now, font selection is independent
from setting the font size.  Please use set-global-staff-size
or layout-set-staff-size to replace this parameter, which you
set to:

    {}

""")

    pango_advanced_warning = _(r"""
LilyPond now uses a different syntax for selecting fonts. The code equivalent
to the previous spelling

\paper {
  #(define fonts
     (make-pango-font-tree
       "roman font"
       "sans font"
       "typewriter font"
       factor))
}

is now

\paper {
  fonts.roman = "roman font"
  fonts.sans = "sans font"
  fonts.typewriter = "typewriter font"
}

convert-ly found an advanced use of make-pango-font-tree that it was not able
to convert automatically.  Please do the update manually.
""")

    pango_re = (rf"(?P<pango>\(make-pango-font-tree\s+"
                rf"(?P<roman>{maybe_funcall_sexpr_re})\s+"
                rf"(?P<sans>{maybe_funcall_sexpr_re})\s+"
                rf"(?P<typewriter>{maybe_funcall_sexpr_re})\s+"
                rf"(?P<factor>{maybe_funcall_sexpr_re})\s*"
                rf"\))")

    def replace_pango(match):
        indent = match.group("indent")
        pango = match.group("pango")
        lines = []
        for family in ("roman", "sans", "typewriter"):
            font = match.group(family)
            if not font.startswith('"'):
                font = "#" + font
            lines.append(f"{indent}fonts.{family} = {font}")
        factor = match.group("factor").split()
        if (
            factor != ["1"]
            and factor != ["(/", "staff-height", "pt", "20)"]
            and factor != ["(/", "staff-height", "20", "pt)"]
        ):
            stderr_write(NOT_SMART % _("factor parameter to make-pango-font-tree"))
            stderr_write(pango_warning.format(match.group("factor")))
            stderr_write(UPDATE_MANUALLY)
        return "\n".join(lines)

    pango_define_re = rf"{indent_re}#\(define\s+fonts\s+{pango_re}\s*\)"
    pango_assign_re = rf"{indent_re}fonts\s*=\s*#{pango_re}"
    s = re.sub(pango_define_re, replace_pango, s, flags=re.MULTILINE)
    s = re.sub(pango_assign_re, replace_pango, s, flags=re.MULTILINE)

    if "make-pango-font-tree" in s:
        stderr_write(NOT_SMART % _("advanced use of make-pango-font-tree"))
        stderr_write(pango_advanced_warning)
        stderr_write(UPDATE_MANUALLY)

    # Convert \lookup to \musicglyph if the argument is not a brace glyph.
    # Also strip outer \override #'(font-encoding . fetaMusic) if found;
    # this is not necessary, but it shortens the input code.  We detect
    # \override #'(font-encoding . fetaBraces) { \lookup ... }
    # as well because many users put redundant braces in markup.
    template = r'''(?x)
       {delim1}
       \\lookup  \s*  \#?  " (?!brace)([^"]+) "
       {delim2}
               '''
    delim1_no_brace = r'''
      \\override  \s*  \#'\(font-encoding \s+ \.  \s+ fetaMusic\)\s*
                      '''
    delim1_brace = delim1_no_brace + r"\{\s*"
    delim2_brace = r"\s*\}"
    repl = r'\\musicglyph "\1"'

    s = re.sub(template.format(delim1=delim1_no_brace, delim2=""), repl, s)
    s = re.sub(template.format(delim1=delim1_brace, delim2=delim2_brace), repl, s)
    s = re.sub(template.format(delim1="", delim2=""), repl, s)

    # Convert make-lookup-markup uses
    s = re.sub(r'\(make-lookup-markup\s+"(?!brace)([^"]+)"\)',
               r'(make-musicglyph-markup "\1")',
               s)

    # Convert (markup #:lookup ...) uses
    s = re.sub(r'#:lookup\s+"(?!brace)([^"]+)"', r'#:musicglyph "\1"', s)

    # For \override
    s = re.sub(r"font-shape\s*=\s*#'caps", "font-variant = #'small-caps", s)
    # For \tweak
    s = re.sub(r"font-shape\s+#'caps", "font-variant #'small-caps", s)
    # For \markup \override
    s = re.sub(r"\(font-shape\s+\.\s+caps\)", "(font-variant . small-caps)", s)

    # Convert \medium markup command to \markup \normal-weight
    # \markup \medium
    s = re.sub(r"\\medium", r"\\normal-weight", s)
    # make-medium-markup
    s = re.sub(r"make-medium-markup", "make-normal-weight-markup", s)
    # (markup #:medium ...)
    s = re.sub(r"#:medium", "#:normal-weight", s)

    # For \override
    s = re.sub(r"font-series\s*=\s*#'medium", "font-series = #'normal", s)
    # For \tweak
    s = re.sub(r"font-series\s+#'medium", "font-series #'normal", s)
    # For \markup \override
    s = re.sub(r"\(font-series\s+\.\s+medium\)", "(font-series . normal)", s)

    if "repeatCommands" in s and ('(volta "' in s or '(volta ,#{' in s):
        stderr_write(NOT_SMART % _("markup in repeatCommands"))
        stderr_write(_(r"""
Markups inside repeatCommands are no longer automatically typeset in
a music font. Use the \volta-number command where needed. Example
replacements:

  \set Score.repeatCommands = #'((volta "ad lib."))
  [does not need conversion]

  \set Score.repeatCommands = #'((volta "1.2."))
  -> \set Score.repeatCommands = #`((volta ,#{ \markup \volta-number "1.2." #}))

  \set Score.repeatCommands = #'((volta "1.2. ad lib."))
  -> \set Score.repeatCommands = #`((volta ,#{ \markup { \volta-number "1.2." ad lib. } #}))
"""))
    return s


# Copied from Pygments
NAME_END_RE = r"(?=\d|[^\w\-]|[\-_][\W\d])"

@rule((2, 25, 5), r"""
font-defaults, text-font-defaults -> property-defaults

Check for identifiers formerly present in 'gregorian.ly'.

\include "gregorian.ly" -> GregorianScore context

\roman -> \serif
font-family = #'roman -> font-family = #'serif
fonts.roman = ... -> fonts.serif = ...
""")
def conv(s):
    s = re.sub(r"(text-)?font-defaults(?=\s*\.\s*[\w-]+\s*=)", "property-defaults", s)
    if "font-defaults" in s:
        stderr_write(NOT_SMART % _("advanced use of (text-)font-defaults"))
        stderr_write(_("""
The text-font-defaults and font-defaults variables have
been merged into a single property-defaults variable.
"""))
        stderr_write(UPDATE_MANUALLY)

    gregorian_warning = _("""
Identifiers formerly in file 'gregorian.ly' are now always defined.  The
following variable or variables in your LilyPond input file have the same
name as one of these identifiers:

  {}

It is recommended to rename them.
""")

    vaticana_score_warning = _(r"""
The use of 'gregorian.ly' is deprecated.  Code like

  \input "gregorian.ly"

  \new VaticanaStaff { ... }

should be replaced with

  \new VaticanaScore {
    \new VaticanaStaff { ... }
  }

  \layout {
    indent = 0
    ragged-last = ##t
  }

""")

    ancient_re = rf'''(?x)
  \\
  ( IJ |
    IIJ |
    ij |
    iij |

    versus |
    responsum |

    virga |
    stropha |
    inclinatum |
    auctum |
    descendens |
    ascendens |
    pes |
    flexa |
    oriscus |
    quilisma |
    deminutum |
    linea |
    cavum |

    virgula |
    divisioMinima |
    divisioMaior |
    divisioMaxima |
    finalis |

    accentus |
    ictus |
    semicirculus |
    circulus |

    augmentum |
    ligature )
  {NAME_END_RE}
'''

    if re.search (r'(?xm) ^ \\include \s+ "gregorian.ly"', s):
        stderr_write(NOT_SMART % _("gregorian.ly to VaticanaScore"))
        stderr_write(vaticana_score_warning)
        stderr_write(UPDATE_MANUALLY)
    else:
        keywords = sorted(set(re.findall(ancient_re, s)))
        if keywords:
            stderr_write(gregorian_warning.format(' '.join(keywords)))

    # For \markup \roman
    s = re.sub(r"\\roman\b", r"\\serif", s)
    # For #(make-roman-markup ...)
    s = re.sub(r"make-roman-markup", "make-serif-markup", s)
    # We don't convert (markup #:roman ...) because there would be a risk of false
    # positives, especially with advanced uses of set-global-fonts that convert-ly
    # might not have been able to replace.
    if m := re.search(r"#:roman\b(?!-)", s):
        stderr_write(NOT_SMART % _(r"possible use of \roman "
                                   "markup command with #(markup ...) macro"))
        stderr_write(_(r"""
convert-ly detected "#:roman" in the input file.  This may be
related to using the \roman markup command with the markup
macro, e.g., #(markup #:roman ...).  If this is the case, convert
#:roman to #:serif.
"""))

    # For \override
    s = re.sub(r"(font-family\s*=\s*[#$]')roman", r"\1serif", s)
    # For \tweak
    s = re.sub(r"(font-family\s+[#$]')roman", r"\1serif", s)
    # For \markup \override
    s = re.sub(r"(\(\s*font-family\s+\.\s+)roman(\s*\))", r"\1serif\2", s)
    # For fonts.roman = ...
    s = re.sub(r"(fonts\s*\.\s*)roman(\s*=\s*)", r"\1serif\2", s)

    return s


@rule((2, 25, 6), r"""
BarCheck -> BarCheckEvent
fonts.roman (etc.) -> property-defaults.fonts.roman (etc.)
\text -> \serif, \sans or \typewriter
""")
def conv(s):
    s = re.sub(r"\bBarCheck\b", r"BarCheckEvent", s)
    s = re.sub(r"((?<!-)(?<!property-defaults\.)\bfonts\s*\.\s*[\w-]+\s*=)", r"property-defaults.\1", s)

    # "text" could easily be a variable name, try to mitigate possible
    # false positives.
    if re.search(rf'\\text{NAME_END_RE}', s) and not 'text =' in s:
       stderr_write(NOT_SMART % r'\text')
       stderr_write(r"""
The \text markup command has been removed.  Instead, use \serif,
\sans or \typewriter, depending on the desired font style.
""")
       stderr_write(UPDATE_MANUALLY)

    return s


@rule((2, 25, 8), r"Warn about new behavior of subdivideBeams")
def conv(s):
    if re.search(r"\bsubdivideBeams\b", s):
        stderr_write(NOT_SMART % _('subdivideBeams'))
        stderr_write(_("""
Context property 'subdivideBeams' used to rely on the value of
'baseMoment' for the minimum subdivision interval.  Now, separate
properties 'minimumBeamSubdivisionInterval',
'maximumBeamSubdivisionInterval', and 'respectIncompleteBeams'
specifically control beam subdivision intervals, whereas 'baseMoment' no
longer has an effect.  The value of 'baseMoment' may implicitly change
whenever a time signature changes or is first set, so preserving past
behavior requires setting 'minimumBeamSubdivisionInterval' to the same
value of 'baseMoment', even at each implicit setting of 'baseMoment',
and setting 'respectIncompleteBeams' to true.
"""))
        stderr_write(UPDATE_MANUALLY)

    return s


@rule((2, 25, 9), r"in-note-padding -> in-note-system-padding")
def conv(s):
    s = re.sub('in-note-padding', 'in-note-system-padding', s)
    return s


@rule((2, 25, 11), r"""
BassFigureContinuation.padding
  -> BassFigureContinuation.bound-details.left.padding
""")
def conv(s):
    s = re.sub(r"BassFigureContinuation\.padding",
               r"BassFigureContinuation.bound-details.left.padding", s)
    return s


@rule((2, 25, 12), r"""
single-digit -> single-number
""")
def conv(s):
    s = re.sub(r"single-digit", r"single-number", s)
    return s


@rule((2, 25, 13), r"""
(ly:set-option 'pixel-format '...)
  -> (ly:set-option 'pixel-format "...")
(ly:set-option 'separate-page-formats '...)
  -> (ly:set-option 'separate-page-formats "...")
(ly:set-option 'tall-page-formats '...)
  -> (ly:set-option 'tall-page-formats "...")
""")
def conv(s):
    s = re.sub(r"([#$]\(\s*ly:set-option\s+'pixel-format\s+)'(.*?)(\s*\))",
               r'\1"\2"\3', s)
    s = re.sub(r"([#$]\(\s*ly:set-option\s+'separate-page-formats\s+)'(.*?)(\s*\))",
               r'\1"\2"\3', s)
    s = re.sub(r"([#$]\(\s*ly:set-option\s+'tall-page-formats\s+)'(.*?)(\s*\))",
               r'\1"\2"\3', s)
    return s


@rule((2, 25, 18), r"""
scripts.upbow -> scripts.uupbow
scripts.downbow -> scripts.udownbow
""")
def conv(s):
  s = s.replace('scripts.upbow', 'scripts.uupbow')
  s = s.replace('scripts.downbow', 'scripts.udownbow')
  return s


# Non-numeric durations like \breve seem unlikely, so we'll ignore them for
# simplicity and revise this if we receive any complaints.
def make_mom_assign_re(old_property_name):
    return (r'\b'
            + old_property_name +
            r'(?P<assignment>\s*=\s*)'
            r'('
            r'\\musicLength\s+'
            r'(?P<head>\d+)'
            r'(?P<dots>\.*)'
            r'(\*(?P<factor_num>\d+)(/(?P<factor_den>\d+))?)?'
            r'|'
            r'#(?P<special_value>(INF-MOMENT|ZERO-MOMENT))'
            r')'
            )

def make_mom_assign_replacer(new_property_name):
    def replacer(match):
        eq = match.group('assignment')
        if match.group('special_value') == "INF-MOMENT":
            return f'{new_property_name}{eq}#+inf.0'
        elif match.group('special_value') == "ZERO-MOMENT":
            return f'{new_property_name}{eq}0'
        head = int(match.group('head'))
        dots = len(match.group('dots'))
        num = int(match.group('factor_num') or 1)
        den = int(match.group('factor_den') or 1)
        if (dots == 0) and (den == 1):
            if head == 1:
                return f'{new_property_name}{eq}{num}'
            # There might be some explanatory value in fractions that are not
            # reduced, so for example, we turn `8*4` into `#4/8` rather than
            # `#1/2`.
            return f'{new_property_name}{eq}#{num}/{head}'
        # We're losing the basic duration (h) anyway, so reduce it.
        f = Fraction(1, head) * Fraction(num, den)
        if dots:
            f *= Fraction((1 << (dots + 1)) - 1, 1 << dots)
        if f.is_integer():
            return f'{new_property_name}{eq}{f}'
        return f'{new_property_name}{eq}#{f}'
    return replacer

@rule((2, 25, 22), r"""
(base-length ... -> (beat-base ...
baseMoment = \musicLength <duration> -> beatBase = #<rational>
(duration-length ... -> (ly:duration->number ...
(ly:duration-length ... -> (ly:duration->moment ...
""")
def conv(s):
    s = re.sub(r'(\(\s*)base-length(\s)', r'\1beat-base\2', s)
    s = re.sub(make_mom_assign_re('baseMoment'),
               make_mom_assign_replacer('beatBase'), s)
    # This isn't necessary, but add `#` in this case for consistency with the
    # beatBase = ... requirement.
    s = re.sub(r'(\\overrideTimeSignatureSettings\s+\d+/\d+\s+)(\d+/\d+)\b',
               r'\1#\2', s)
    s = re.sub(r'(\(\s*)duration-length(\s)', r'\1ly:duration->number\2', s)
    s = re.sub(r'(\(\s*)ly:duration-length(\s)', r'\1ly:duration->moment\2', s)
    return s


@rule((2, 25, 23), r"""
maximumBeamSubdivisionInterval -> beamMaximumSubdivision
minimumBeamSubdivisionInterval -> beamMinimumSubdivision
tempoWholesPerMinute = \musicLength <duration> -> ... = #<rational>
voltaSpannerDuration = \musicLength <duration> -> ... = #<rational>
minimumPageTurnLength -> pageTurnMinimumRestLength
minimumRepeatLengthForPageTurn -> pageTurnMinimumRepeatLength
completionUnit = \musicLength <duration> -> completionUnit = #<rational>
gridInterval = \musicLength <duration> -> ... = #<rational>
proportionalNotationDuration = \musicLength <duration> -> ... = #<rational>
tupletSpannerDuration = \musicLength <duration> -> ... = #<rational>
(calculate-compound-base-beat -> (...-as-moment
(calculate-compound-measure-length -> (...-as-moment
measureLength -> measureLengthAsMoment
""")
def conv(s):
    s = re.sub(make_mom_assign_re('maximumBeamSubdivisionInterval'),
               make_mom_assign_replacer('beamMaximumSubdivision'), s)
    s = re.sub(r'(\\unset\s+)maximumBeamSubdivisionInterval',
               r'\1beamMaximumSubdivision', s)
    s = re.sub(make_mom_assign_re('minimumBeamSubdivisionInterval'),
               make_mom_assign_replacer('beamMinimumSubdivision'), s)
    s = re.sub(r'(\\unset\s+)minimumBeamSubdivisionInterval',
               r'\1beamMinimumSubdivision', s)
    s = re.sub(r'\btempoWholesPerMinute\b', r'tempoWholesPerMinuteAsMoment', s)
    s = re.sub(make_mom_assign_re('tempoWholesPerMinuteAsMoment'),
               make_mom_assign_replacer('tempoWholesPerMinute'), s)
    s = re.sub(r'\bvoltaSpannerDuration\b', r'voltaSpannerDurationAsMoment', s)
    s = re.sub(make_mom_assign_re('voltaSpannerDurationAsMoment'),
               make_mom_assign_replacer('voltaSpannerDuration'), s)
    s = re.sub(make_mom_assign_re('minimumPageTurnLength'),
               make_mom_assign_replacer('pageTurnMinimumRestLength'), s)
    s = re.sub(make_mom_assign_re('minimumRepeatLengthForPageTurn'),
               make_mom_assign_replacer('pageTurnMinimumRepeatLength'), s)
    s = re.sub(r'\bcompletionUnit\b', r'completionUnitAsMoment', s)
    s = re.sub(make_mom_assign_re('completionUnitAsMoment'),
               make_mom_assign_replacer('completionUnit'), s)
    s = re.sub(r'\bgridInterval\b', r'gridIntervalAsMoment', s)
    s = re.sub(make_mom_assign_re('gridIntervalAsMoment'),
               make_mom_assign_replacer('gridInterval'), s)
    s = re.sub(r'\bproportionalNotationDuration\b',
               r'proportionalNotationDurationAsMoment', s)
    s = re.sub(make_mom_assign_re('proportionalNotationDurationAsMoment'),
               make_mom_assign_replacer('proportionalNotationDuration'), s)
    s = re.sub(r'\btupletSpannerDuration\b',
               r'tupletSpannerDurationAsMoment', s)
    s = re.sub(make_mom_assign_re('tupletSpannerDurationAsMoment'),
               make_mom_assign_replacer('tupletSpannerDuration'), s)
    s = re.sub(r'(\(\s*)calculate-compound-base-beat(\s)',
               r'\1calculate-compound-beat-base-as-moment\2', s)
    s = re.sub(r'(\(\s*)calculate-compound-measure-length(\s)',
               r'\1calculate-compound-measure-length-as-moment\2', s)
    s = re.sub(r'\bmeasureLength\b', r'measureLengthAsMoment', s)
    s = re.sub(make_mom_assign_re('measureLengthAsMoment'),
               make_mom_assign_replacer('measureLength'), s)
    return s


@rule((2, 25, 24), r"""
Warn about change in partcombine music types and event classes.
Warn about change in format of partcombine state machine tables.
\pushContextProperty path val -> \pushContextProperty path \set path = val
""")
def conv(s):
    if re.search(r'(Unisono|SoloOne|SoloTwo)Event', s):
        stderr_write(NOT_SMART % _(r'Advanced use of partcombine music types.'))
        stderr_write(_(r"""
UnisonoEvent, SoloOneEvent, and UnisonoEvent have been merged into
PartCombineEvent, with different values for property 'part-combine-status:
'unisono, 'solo1, 'solo2 respectively.
"""))
        stderr_write(UPDATE_MANUALLY)
    if re.search(r'(unisono|solo-one|solo-two)-event', s):
        stderr_write(NOT_SMART % _(r'Advanced use of partcombine event classes.'))
        stderr_write(_(r"""
unisono-event, solo-one-event, and unisono-event have been removed. Instead,
use part-combine-event with different values for property 'part-combine-status:
'unisono, 'solo1, 'solo2 respectively.
"""))
        stderr_write(UPDATE_MANUALLY)
    if re.search(r'make-part-combine-(marks|context-changes)', s):
        stderr_write(NOT_SMART % _(r"""
Advanced use of make-part-combine-marks or make-part-combine-context-changes.
"""))
        stderr_write(_(r"""
The format of values returned by all partcombine state machine tables was
changed:

  1. 'next-label' symbols moved from LAST to FIRST
  2. Values are now always proper lists (changed from dotted pairs for
     context-changes table)

The order of arguments to make-part-combine-marks was reversed (but not for
make-part-combine-context-changes). The state-machine argument is now optional,
and expects a procedure rather than a state-machine table. For equivalent
behavior, pass:
        (traverse-state-machine <your-state-machine-table>)
as the second argument.

Refer to part-combiner.scm for more information.
"""))
        stderr_write(UPDATE_MANUALLY)
    s = re.sub(r'(\\pushContextProperty)\s+(\S+)(\s+)',
               r'\1 \2 \\set \2 = ', s)
    return s


@rule((2, 25, 25), r"""
ellipsis-direction -> passage-direction
remove max-/minimumBeamSubdivisionInterval
""")
def conv(s):
    s = re.sub(r'\bellipsis-direction\b', r'passage-direction', s)
    if 'maximumBeamSubdivisionInterval' in s:
        stderr_write(NOT_SMART % 'maximumBeamSubdivisionInterval')
        stderr_write(FROM_TO % ('maximumBeamSubdivisionInterval',
                                'beamMaximumSubdivision'))
        stderr_write(UPDATE_MANUALLY)
        raise FatalConversionError()
    if 'minimumBeamSubdivisionInterval' in s:
        stderr_write(NOT_SMART % 'minimumBeamSubdivisionInterval')
        stderr_write(FROM_TO % ('minimumBeamSubdivisionInterval',
                                'beamMinimumSubdivision'))
        stderr_write(UPDATE_MANUALLY)
        raise FatalConversionError()
    return s


@rule((2, 25, 26), r"""
(empty-music) -> (make-music 'Music)
""")
def conv(s):
    s = re.sub(r'(\(\s*)empty-music(\s*\))', "(make-music 'Music)", s)
    return s


@rule((2, 25, 28), r"""
timeSignatureFraction -> timeSignature
""")
def conv(s):
    s = re.sub(r'\btimeSignatureFraction\s*=', r'timeSignature =', s)
    return s


# Guidelines to write rules (please keep this at the end of this file)
#
# - keep at most one rule per version; if several conversions should be done,
#   concatenate them into a single "conv" function
#
# - enclose strings to be localized with `_(' and  `)'
