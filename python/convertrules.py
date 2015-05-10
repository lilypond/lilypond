# -*- coding: utf-8 -*-
# (setq py-indent-offset 4)


import string
import re
import sys
import lilylib

_ = lilylib._


NOT_SMART = "\n" + _ ("Not smart enough to convert %s.") + "\n"
UPDATE_MANUALLY = _ ("Please refer to the manual for details, and update manually.") + "\n"
FROM_TO = _ ("%s has been replaced by %s") + "\n"


class FatalConversionError:
    pass

conversions = []
stderr_write = lilylib.stderr_write

def warning (str):
    stderr_write (_ ("warning: %s") % str)

# Decorator to make rule syntax simpler
def rule (version, message):
    """
    version: a LilyPond version tuple like (2, 11, 50)
    message: the message that describes the conversion.

    This decorator adds its function together with the version and the
    message to the global conversions list.  (It doesn't need to return
    the function as it isn't used directly anyway.)

    A conversion rule using this decorator looks like this:

    @rule ((1, 2, 3), "convert foo to bar")
    def conv(str):
        str = str.replace('foo', 'bar')
        return str

    """
    def dec(f):
        conversions.append ((version, f, message))
    return dec


@rule ((0, 1, 9), _ ('\\header { key = concat + with + operator }'))
def conv(str):
    if re.search ('\\\\multi', str):
        stderr_write (NOT_SMART % "\\multi")
    return str


@rule ((0, 1, 19), _ ('deprecated %s') % '\\octave')
def conv (str):
    if re.search ('\\\\octave', str):
        stderr_write (NOT_SMART % "\\octave")
        stderr_write (UPDATE_MANUALLY)
    #   raise FatalConversionError ()
    return str


@rule ((0, 1, 20), _ ('deprecated \\textstyle, new \\key syntax'))
def conv (str):
    str = re.sub ('\\\\textstyle([^;]+);',
                             '\\\\property Lyrics . textstyle = \\1', str)
    # harmful to current .lys
    # str = re.sub ('\\\\key([^;]+);', '\\\\accidentals \\1;', str)
    return str


@rule ((0, 1, 21), '\\musical_pitch -> \\musicalpitch, \\meter -> \\time')
def conv (str):
    str = re.sub ('\\\\musical_pitch', '\\\\musicalpitch',str)
    str = re.sub ('\\\\meter', '\\\\time',str)
    return str


@rule ((1, 0, 0), _ ("bump version for release"))
def conv (str):
    return str


@rule ((1, 0, 1), '\\accidentals -> \\keysignature, specialaccidentals -> keyoctaviation')
def conv (str):
    str = re.sub ('\\\\accidentals', '\\\\keysignature',str)
    str = re.sub ('specialaccidentals *= *1', 'keyoctaviation = 0',str)
    str = re.sub ('specialaccidentals *= *0', 'keyoctaviation = 1',str)
    return str


@rule ((1, 0, 2), _ ('\\header { key = concat + with + operator }'))
def conv(str):
    if re.search ('\\\\header', str):
        stderr_write (NOT_SMART % _ ("new \\header format"))
    return str


@rule ((1, 0, 3), '\\melodic -> \\notes')
def conv(str):
    str =  re.sub ('\\\\melodic([^a-zA-Z])', '\\\\notes\\1',str)
    return str


@rule ((1, 0, 4), 'default_{paper,midi}')
def conv(str):
    str =  re.sub ('default_paper *=', '',str)
    str =  re.sub ('default_midi *=', '',str)
    return str


@rule ((1, 0, 5), 'ChoireStaff -> ChoirStaff')
def conv(str):
    str =  re.sub ('ChoireStaff', 'ChoirStaff',str)
    str =  re.sub ('\\\\output', 'output = ',str)
    return str


@rule ((1, 0, 6), 'foo = \\translator {\\type .. } ->\\translator {\\type ..; foo; }')
def conv(str):
    if re.search ('[a-zA-Z]+ = *\\translator',str):
        stderr_write (NOT_SMART % _ ("\\translator syntax"))
    #   raise FatalConversionError ()
    return str


@rule ((1, 0, 7), '\\lyric -> \\lyrics')
def conv(str):
    str =  re.sub ('\\\\lyrics*', '\\\\lyrics',str)
    return str


@rule ((1, 0, 10), '[2/3 ]1/1 -> \\times 2/3 ')
def conv(str):
    str =  re.sub ('\\\\\\[/3+', '\\\\times 2/3 { ',str)
    str =  re.sub ('\\[/3+', '\\\\times 2/3 { [',str)
    str =  re.sub ('\\\\\\[([0-9/]+)', '\\\\times \\1 {',str)
    str =  re.sub ('\\[([0-9/]+)', '\\\\times \\1 { [',str)
    str =  re.sub ('\\\\\\]([0-9/]+)', '}', str)
    str =  re.sub ('\\\\\\]', '}',str)
    str =  re.sub ('\\]([0-9/]+)', '] }', str)
    return str


@rule ((1, 0, 12), 'Chord syntax stuff')
def conv(str):
    return str


@rule ((1, 0, 13), '<a ~ b> c -> <a b> ~ c')
def conv(str):
    str =  re.sub ('<([^>~]+)~([^>]*)>','<\\1 \\2> ~', str)
    return str


@rule ((1, 0, 14), '<[a b> <a b]>c -> [<a b> <a b>]')
def conv(str):
    str =  re.sub ('<\\[','[<', str)
    str =  re.sub ('\\]>','>]', str)
    return str


@rule ((1, 0, 16), '\\type -> \\context, textstyle -> textStyle')
def conv(str):
    str =  re.sub ('\\\\type([^\n]*engraver)','\\\\TYPE\\1', str)
    str =  re.sub ('\\\\type([^\n]*performer)','\\\\TYPE\\1', str)
    str =  re.sub ('\\\\type','\\\\context', str)
    str =  re.sub ('\\\\TYPE','\\\\type', str)
    str =  re.sub ('textstyle','textStyle', str)
    return str


@rule ((1, 0, 18), _ ('\\repeat NUM Music Alternative -> \\repeat FOLDSTR Music Alternative'))
def conv(str):
    if re.search ('\\\\repeat',str):
        stderr_write (NOT_SMART % "\\repeat")
    #   raise FatalConversionError ()
    return str


@rule ((1, 0, 19), 'fontsize -> fontSize, midi_instrument -> midiInstrument, SkipBars -> skipBars')
def conv(str):
    str =  re.sub ('SkipBars','skipBars', str)
    str =  re.sub ('fontsize','fontSize', str)
    str =  re.sub ('midi_instrument','midiInstrument', str)
    return str


@rule ((1, 0, 20), '{,tie,slur}ydirection -> {v,tieV,slurV}erticalDirection')
def conv(str):
    str =  re.sub ('tieydirection','tieVerticalDirection', str)
    str =  re.sub ('slurydirection','slurVerticalDirection', str)
    str =  re.sub ('ydirection','verticalDirection', str)
    return str


@rule ((1, 0, 21), 'hshift -> horizontalNoteShift')
def conv(str):
    str =  re.sub ('hshift','horizontalNoteShift', str)
    return str


@rule ((1, 1, 52), _ ('deprecate %s') % '\\grouping')
def conv(str):
    str =  re.sub ('\\\\grouping[^;]*;','', str)
    return str


@rule ((1, 1, 55), '\\wheel -> \\coda')
def conv(str):
    str =  re.sub ('\\\\wheel','\\\\coda', str)
    return str


@rule ((1, 1, 65), 'slurdash -> slurDash, keyoctaviation -> keyOctaviation')
def conv(str):
    str =  re.sub ('keyoctaviation','keyOctaviation', str)
    str =  re.sub ('slurdash','slurDash', str)
    return str


@rule ((1, 1, 66), 'semi -> volta')
def conv(str):
    str =  re.sub ('\\\\repeat *\"?semi\"?','\\\\repeat "volta"', str)
    return str


@rule ((1, 1, 67), 'beamAuto -> noAutoBeaming')
def conv(str):
    str =  re.sub ('\"?beamAuto\"? *= *\"?0?\"?','noAutoBeaming = "1"', str)
    return str


@rule ((1, 2, 0), 'automaticMelismas -> automaticMelismata')
def conv(str):
    str =  re.sub ('automaticMelismas', 'automaticMelismata', str)
    return str


@rule ((1, 2, 1), 'dynamicDir -> dynamicDirection')
def conv(str):
    str =  re.sub ('dynamicDir\\b', 'dynamicDirection', str)
    return str


@rule ((1, 3, 4), '\\cadenza -> \\cadenza{On|Off}')
def conv(str):
    str =  re.sub ('\\\\cadenza *0 *;', '\\\\cadenzaOff', str)
    str =  re.sub ('\\\\cadenza *1 *;', '\\\\cadenzaOn', str)
    return str


@rule ((1, 3, 5), 'beamAuto moment properties')
def conv (str):
    str = re.sub ('"?beamAuto([^"=]+)"? *= *"([0-9]+)/([0-9]+)" *;*',
                  'beamAuto\\1 = #(make-moment \\2 \\3)',
                  str)
    return str


@rule ((1, 3, 17), 'stemStyle -> flagStyle')
def conv (str):
    str = re.sub ('stemStyle',
                  'flagStyle',
                  str)
    return str


@rule ((1, 3, 18), 'staffLineLeading -> staffSpace')
def conv (str):
    str = re.sub ('staffLineLeading',
                  'staffSpace',
                  str)
    return str


@rule ((1, 3, 23), _ ('deprecate %s ') % '\\repetitions')
def conv(str):
    if re.search ('\\\\repetitions',str):
        stderr_write (NOT_SMART % "\\repetitions")
    #   raise FatalConversionError ()
    return str


@rule ((1, 3, 35), 'textEmptyDimension -> textNonEmpty')
def conv (str):
    str = re.sub ('textEmptyDimension *= *##t',
                  'textNonEmpty = ##f',
                  str)
    str = re.sub ('textEmptyDimension *= *##f',
                  'textNonEmpty = ##t',
                  str)
    return str


@rule ((1, 3, 38), '\musicalpitch { a b c } -> #\'(a b c)')
def conv (str):
    str = re.sub ("([a-z]+)[ \t]*=[ \t]*\\\\musicalpitch *{([- 0-9]+)} *\n",
                  "(\\1 . (\\2))\n", str)
    str = re.sub ("\\\\musicalpitch *{([0-9 -]+)}",
                  "\\\\musicalpitch #'(\\1)", str)
    if re.search ('\\\\notenames',str):
        stderr_write (NOT_SMART % _ ("new \\notenames format"))
    return str


@rule ((1, 3, 39), '\\key A ;  ->\\key a;')
def conv (str):
    def replace (match):
        return '\\key %s;' % match.group (1).lower ()

    str = re.sub ("\\\\key ([^;]+);",  replace, str)
    return str


@rule ((1, 3, 41), '[:16 c4 d4 ] -> \\repeat "tremolo" 2 { c16 d16 }')
def conv (str):
    if re.search ('\\[:',str):
        stderr_write (NOT_SMART % _ ("new tremolo format"))
    return str


@rule ((1, 3, 42), _ ('Staff_margin_engraver deprecated, use Instrument_name_engraver'))
def conv (str):
    str = re.sub ('Staff_margin_engraver' , 'Instrument_name_engraver', str)
    return str


@rule ((1, 3, 49), 'noteHeadStyle value: string -> symbol')
def conv (str):
    str = re.sub ('note[hH]eadStyle\\s*=\\s*"?(\\w+)"?' , "noteHeadStyle = #'\\1", str)
    return str


@rule ((1, 3, 58), 'noteHeadStyle value: string -> symbol')
def conv (str):
    if re.search ('\\\\keysignature', str):
        stderr_write (NOT_SMART % '\\keysignature')
    return str


@rule ((1, 3, 59), '\key X ; -> \key X major; ')
def conv (str):
    str = re.sub (r"""\\key *([a-z]+) *;""", r"""\\key \1 \major;""",str);
    return str


@rule ((1, 3, 68), 'latexheaders = "\\input global" -> latexheaders = "global"')
def conv (str):
    str = re.sub (r'latexheaders *= *"\\\\input ',
                  'latexheaders = "',
                  str)
    return str


# TODO: lots of other syntax changes should be done here as well
@rule ((1, 3, 92), 'basicXXXProperties -> XXX, Repeat_engraver -> Volta_engraver')
def conv (str):
    str = re.sub ('basicCollisionProperties', 'NoteCollision', str)
    str = re.sub ('basicVoltaSpannerProperties' , "VoltaBracket", str)
    str = re.sub ('basicKeyProperties' , "KeySignature", str)

    str = re.sub ('basicClefItemProperties' ,"Clef", str)


    str = re.sub ('basicLocalKeyProperties' ,"Accidentals", str)
    str = re.sub ('basicMarkProperties' ,"Accidentals", str)
    str = re.sub ('basic([A-Za-z_]+)Properties', '\\1', str)

    str = re.sub ('Repeat_engraver' ,'Volta_engraver', str)
    return str


@rule ((1, 3, 93), _ ('change property definition case (eg. onevoice -> oneVoice)'))
def conv (str):
    # Ugh, but meaning of \stemup changed too
    # maybe we should do \stemup -> \stemUp\slurUp\tieUp ?
    str = re.sub ('\\\\stemup', '\\\\stemUp', str)
    str = re.sub ('\\\\stemdown', '\\\\stemDown', str)
    str = re.sub ('\\\\stemboth', '\\\\stemBoth', str)

    str = re.sub ('\\\\slurup', '\\\\slurUp', str)
    str = re.sub ('\\\\slurboth', '\\\\slurBoth', str)
    str = re.sub ('\\\\slurdown', '\\\\slurDown', str)
    str = re.sub ('\\\\slurdotted', '\\\\slurDotted', str)
    str = re.sub ('\\\\slurnormal', '\\\\slurNoDots', str)

    str = re.sub ('\\\\shiftoff', '\\\\shiftOff', str)
    str = re.sub ('\\\\shifton', '\\\\shiftOn', str)
    str = re.sub ('\\\\shiftonn', '\\\\shiftOnn', str)
    str = re.sub ('\\\\shiftonnn', '\\\\shiftOnnn', str)

    str = re.sub ('\\\\onevoice', '\\\\oneVoice', str)
    str = re.sub ('\\\\voiceone', '\\\\voiceOne', str)
    str = re.sub ('\\\\voicetwo', '\\\\voiceTwo', str)
    str = re.sub ('\\\\voicethree', '\\\\voiceThree', str)
    str = re.sub ('\\\\voicefour', '\\\\voiceFour', str)

    # I don't know exactly when these happened...
    # ugh, we lose context setting here...
    str = re.sub ('\\\\property *[^ ]*verticalDirection[^=]*= *#?"?(1|(\\\\up))"?', '\\\\stemUp\\\\slurUp\\\\tieUp', str)
    str = re.sub ('\\\\property *[^ ]*verticalDirection[^=]*= *#?"?((-1)|(\\\\down))"?', '\\\\stemDown\\\\slurDown\\\\tieDown', str)
    str = re.sub ('\\\\property *[^ ]*verticalDirection[^=]*= *#?"?(0|(\\\\center))"?', '\\\\stemBoth\\\\slurBoth\\\\tieBoth', str)

    str = re.sub ('verticalDirection[^=]*= *#?"?(1|(\\\\up))"?', 'Stem \\\\override #\'direction = #0\nSlur \\\\override #\'direction = #0\n Tie \\\\override #\'direction = #1', str)
    str = re.sub ('verticalDirection[^=]*= *#?"?((-1)|(\\\\down))"?', 'Stem \\\\override #\'direction = #0\nSlur \\\\override #\'direction = #0\n Tie \\\\override #\'direction = #-1', str)
    str = re.sub ('verticalDirection[^=]*= *#?"?(0|(\\\\center))"?', 'Stem \\\\override #\'direction = #0\nSlur \\\\override #\'direction = #0\n Tie \\\\override #\'direction = #0', str)

    str = re.sub ('\\\\property *[^ .]*[.]?([a-z]+)VerticalDirection[^=]*= *#?"?(1|(\\\\up))"?', '\\\\\\1Up', str)
    str = re.sub ('\\\\property *[^ .]*[.]?([a-z]+)VerticalDirection[^=]*= *#?"?((-1)|(\\\\down))"?', '\\\\\\1Down', str)
    str = re.sub ('\\\\property *[^ .]*[.]?([a-z]+)VerticalDirection[^=]*= *#?"?(0|(\\\\center))"?', '\\\\\\1Both', str)

    # (lacks capitalization slur -> Slur)
    str = re.sub ('([a-z]+)VerticalDirection[^=]*= *#?"?(1|(\\\\up))"?', '\\1 \\\\override #\'direction = #1', str)
    str = re.sub ('([a-z]+)VerticalDirection[^=]*= *#?"?((-1)|(\\\\down))"?', '\\1 \\override #\'direction = #-1', str)
    str = re.sub ('([a-z]+)VerticalDirection[^=]*= *#?"?(0|(\\\\center))"?', '\\1 \\\\override #\'direction = #0', str)

    ## dynamic..
    str = re.sub ('\\\\property *[^ .]*[.]?dynamicDirection[^=]*= *#?"?(1|(\\\\up))"?', '\\\\dynamicUp', str)
    str = re.sub ('\\\\property *[^ .]*[.]?dyn[^=]*= *#?"?((-1)|(\\\\down))"?', '\\\\dynamicDown', str)
    str = re.sub ('\\\\property *[^ .]*[.]?dyn[^=]*= *#?"?(0|(\\\\center))"?', '\\\\dynamicBoth', str)

    str = re.sub ('\\\\property *[^ .]*[.]?([a-z]+)Dash[^=]*= *#?"?(0|(""))"?', '\\\\\\1NoDots', str)
    str = re.sub ('\\\\property *[^ .]*[.]?([a-z]+)Dash[^=]*= *#?"?([1-9]+)"?', '\\\\\\1Dotted', str)

    str = re.sub ('\\\\property *[^ .]*[.]?noAutoBeaming[^=]*= *#?"?(0|(""))"?', '\\\\autoBeamOn', str)
    str = re.sub ('\\\\property *[^ .]*[.]?noAutoBeaming[^=]*= *#?"?([1-9]+)"?', '\\\\autoBeamOff', str)
    return str


@rule ((1, 3, 97), 'ChordName -> ChordNames')
def conv (str):
    str = re.sub ('ChordNames*', 'ChordNames', str)
    if re.search ('\\\\textscript "[^"]* *"[^"]*"', str):
        stderr_write (NOT_SMART % _ ("new \\textscript markup text"))

    str = re.sub ('\\textscript +("[^"]*")', '\\textscript #\\1', str)
    return str

# TODO: add lots of these

@rule ((1, 3, 98), 'CONTEXT.textStyle -> GROB.#font-style ')
def conv (str):
    str = re.sub ('\\\\property *"?Voice"? *[.] *"?textStyle"? *= *"([^"]*)"', '\\\\property Voice.TextScript \\\\set #\'font-style = #\'\\1', str)
    str = re.sub ('\\\\property *"?Lyrics"? *[.] *"?textStyle"? *= *"([^"]*)"', '\\\\property Lyrics.LyricText \\\\set #\'font-style = #\'\\1', str)

    str = re.sub ('\\\\property *"?([^.]+)"? *[.] *"?timeSignatureStyle"? *= *"([^"]*)"', '\\\\property \\1.TimeSignature \\\\override #\'style = #\'\\2', str)

    str = re.sub ('"?timeSignatureStyle"? *= *#?""', 'TimeSignature \\\\override #\'style = ##f', str)

    str = re.sub ('"?timeSignatureStyle"? *= *#?"([^"]*)"', 'TimeSignature \\\\override #\'style = #\'\\1', str)

    str = re.sub ('#\'style *= #*"([^"])"', '#\'style = #\'\\1', str)

    str = re.sub ('\\\\property *"?([^.]+)"? *[.] *"?horizontalNoteShift"? *= *"?#?([-0-9]+)"?', '\\\\property \\1.NoteColumn \\\\override #\'horizontal-shift = #\\2', str)

    # ugh
    str = re.sub ('\\\\property *"?([^.]+)"? *[.] *"?flagStyle"? *= *""', '\\\\property \\1.Stem \\\\override #\'flag-style = ##f', str)

    str = re.sub ('\\\\property *"?([^.]+)"? *[.] *"?flagStyle"? *= *"([^"]*)"', '\\\\property \\1.Stem \\\\override #\'flag-style = #\'\\2', str)
    return str


@rule ((1, 3, 102), 'beamAutoEnd -> autoBeamSettings \\push (end * * * *)')
def conv (str):
    str = re.sub ('"?beamAutoEnd_([0-9]*)"? *= *(#\\([^)]*\\))', 'autoBeamSettings \\push #\'(end 1 \\1 * *) = \\2', str)
    str = re.sub ('"?beamAutoBegin_([0-9]*)"? *= *(#\\([^)]*\))', 'autoBeamSettings \\push #\'(begin 1 \\1 * *) = \\2', str)
    str = re.sub ('"?beamAutoEnd"? *= *(#\\([^)]*\\))', 'autoBeamSettings \\push #\'(end * * * *) = \\1', str)
    str = re.sub ('"?beamAutoBegin"? *= *(#\\([^)]*\\))', 'autoBeamSettings \\push #\'(begin * * * *) = \\1', str)
    return str


@rule ((1, 3, 111), '\\push -> \\override, \\pop -> \\revert')
def conv (str):
    str = re.sub ('\\\\push', '\\\\override', str)
    str = re.sub ('\\\\pop', '\\\\revert', str)
    return str


@rule ((1, 3, 113), 'LyricVoice -> LyricsVoice')
def conv (str):
    str = re.sub ('LyricVoice', 'LyricsVoice', str)
    # old fix
    str = re.sub ('Chord[Nn]ames*.Chord[Nn]ames*', 'ChordNames.ChordName', str)
    str = re.sub ('Chord[Nn]ames([ \t\n]+\\\\override)', 'ChordName\\1', str)
    return str


def regularize_id (str):
    s = ''
    lastx = ''
    for x in str:
        if x == '_':
            lastx = x
            continue
        elif x in string.digits:
            x = chr(ord (x) - ord ('0')  +ord ('A'))
        elif x not in string.letters:
            x = 'x'
        elif x in string.lowercase and lastx == '_':
            x = x.upper ()
        s = s + x
        lastx = x
    return s


@rule ((1, 3, 117), _ ('identifier names: %s') % '$!foo_bar_123 -> xfooBarABC')
def conv (str):
    def regularize_dollar_reference (match):
        return regularize_id (match.group (1))
    def regularize_assignment (match):
        return '\n' + regularize_id (match.group (1)) + ' = '
    str = re.sub ('\$([^\t\n ]+)', regularize_dollar_reference, str)
    str = re.sub ('\n([^ \t\n]+)[ \t]*= *', regularize_assignment, str)
    return str


@rule ((1, 3, 120), 'paper_xxx -> paperXxxx, pedalup -> pedalUp.')
def conv (str):
    def regularize_paper (match):
        return regularize_id (match.group (1))
    str = re.sub ('(paper_[a-z]+)', regularize_paper, str)
    str = re.sub ('sustainup', 'sustainUp', str)
    str = re.sub ('nobreak', 'noBreak', str)
    str = re.sub ('sustaindown', 'sustainDown', str)
    str = re.sub ('sostenutoup', 'sostenutoUp', str)
    str = re.sub ('sostenutodown', 'sostenutoDown', str)
    str = re.sub ('unachorda', 'unaChorda', str)
    str = re.sub ('trechorde', 'treChorde', str)
    return str


@rule ((1, 3, 122), 'drarnChords -> chordChanges, \\musicalpitch -> \\pitch')
def conv (str):
    str = re.sub ('drarnChords', 'chordChanges', str)
    str = re.sub ('\\musicalpitch', '\\pitch', str)
    return str


@rule ((1, 3, 136), 'ly-X-elt-property -> ly-X-grob-property')
def conv (str):
    str = re.sub ('ly-([sg])et-elt-property', 'ly-\\1et-grob-property', str)
    return str


@rule ((1, 3, 138), _ ('point-and-click argument changed to procedure.'))
def conv (str):
    str = re.sub ('point-and-click +#t', 'point-and-click line-column-location', str)
    return str


@rule ((1, 3, 138), 'followThread -> followVoice.')
def conv (str):
    str = re.sub ('followThread', 'followVoice', str)
    str = re.sub ('Thread.FollowThread', 'Voice.VoiceFollower', str)
    str = re.sub ('FollowThread', 'VoiceFollower', str)
    return str


@rule ((1, 3, 139), 'font-point-size -> font-design-size.')
def conv (str):
    str = re.sub ('font-point-size', 'font-design-size', str)
    return str


@rule ((1, 3, 141), 'xNoDots -> xSolid')
def conv (str):
    str = re.sub ('([a-zA-Z]*)NoDots', '\\1Solid', str)
    return str


@rule ((1, 3, 144), 'Chorda -> Corda')
def conv (str):
    str = re.sub ('([Cc])hord([ea])', '\\1ord\\2', str)
    return str


@rule ((1, 3, 145), 'ContextNameXxxxVerticalExtent -> XxxxVerticalExtent')
def conv (str):
    str = re.sub ('([A-Za-z]+)MinimumVerticalExtent', 'MinimumV@rticalExtent', str)
    str = re.sub ('([A-Za-z]+)ExtraVerticalExtent', 'ExtraV@rticalExtent', str)
    str = re.sub ('([A-Za-z]+)VerticalExtent', 'VerticalExtent', str)
    str = re.sub ('ExtraV@rticalExtent', 'ExtraVerticalExtent', str)
    str = re.sub ('MinimumV@rticalExtent', 'MinimumVerticalExtent', str)
    return str


@rule ((1, 3, 146), _('semicolons removed'))
def conv (str):
    str = re.sub ('\\\\key[ \t]*;', '\\key \\default;', str)
    str = re.sub ('\\\\mark[ \t]*;', '\\mark \\default;', str)

    # Make sure groups of more than one ; have space before
    # them, so that non of them gets removed by next rule
    str = re.sub ("([^ \n\t;]);(;+)", "\\1 ;\\2", str)

    # Only remove ; that are not after spaces, # or ;
    # Otherwise  we interfere with Scheme comments,
    # which is badbadbad.
    str = re.sub ("([^ \t;#]);", "\\1", str)
    return str


@rule ((1, 3, 147), 'default-neutral-direction -> neutral-direction')
def conv (str):
    str = re.sub ('default-neutral-direction', 'neutral-direction',str)
    return str


@rule ((1, 3, 148), '"(align" -> "(axis", "(rows" -> "(columns"')
def conv (str):
    str = re.sub ('\(align', '(axis', str)
    str = re.sub ('\(rows', '(columns', str)
    return str


@rule ((1, 5, 33), 'SystemStartDelimiter -> systemStartDelimiter')
def conv (str):
    str = re.sub ('SystemStartDelimiter', 'systemStartDelimiter', str)
    return str


@rule ((1, 5, 38), 'arithmetic... -> spacing...')
def conv (str):
    str = re.sub ('arithmetic-multiplier', 'spacing-increment', str)
    str = re.sub ('arithmetic-basicspace', 'shortest-duration-space', str)
    return str


# 40 ?
@rule ((1, 5, 40), _ ('%s property names') % 'breakAlignOrder')
def conv (str):

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
        props =  match.group (1)
        for (k,v) in break_dict.items():
            props = re.sub (k, v, props)
        return  "breakAlignOrder = #'(%s)" % props

    str = re.sub ("breakAlignOrder *= *#'\\(([a-z_\n\tA-Z ]+)\\)",
                  func, str)
    return str


@rule ((1, 5, 49), 'noAutoBeaming -> autoBeaming')
def conv (str):
    str = re.sub ('noAutoBeaming *= *##f', 'autoBeaming = ##t', str)
    str = re.sub ('noAutoBeaming *= *##t', 'autoBeaming = ##f', str)
    return str


@rule ((1, 5, 52), 'tuplet-X-visibility -> X-visibility')
def conv (str):
    str = re.sub ('tuplet-bracket-visibility', 'bracket-visibility', str)
    str = re.sub ('tuplet-number-visibility', 'number-visibility', str)
    return str


@rule ((1, 5, 56), 'Pitch::transpose -> ly-transpose-pitch')
def conv (str):
    str = re.sub ('Pitch::transpose', 'ly-transpose-pitch', str)
    return str


@rule ((1, 5, 58), _ ('deprecate %s') % 'textNonEmpty')
def conv (str):
    str = re.sub ('textNonEmpty *= *##t', "TextScript \\set #'no-spacing-rods = ##f", str)
    str = re.sub ('textNonEmpty *= *##f', "TextScript \\set #'no-spacing-rods = ##t", str)
    return str


@rule ((1, 5, 59), 'XxxxVerticalExtent -> xxxVerticalExtent')
def conv (str):
    str = re.sub ('MinimumVerticalExtent', 'minimumV@rticalExtent', str)
    str = re.sub ('minimumVerticalExtent', 'minimumV@rticalExtent', str)
    str = re.sub ('ExtraVerticalExtent', 'extraV@rticalExtent', str)
    str = re.sub ('extraVerticalExtent', 'extraV@rticalExtent', str)
    str = re.sub ('VerticalExtent', 'verticalExtent', str)
    str = re.sub ('extraV@rticalExtent', 'extraVerticalExtent', str)
    str = re.sub ('minimumV@rticalExtent', 'minimumVerticalExtent', str)
    return str


@rule ((1, 5, 62), 'visibility-lambda -> break-visibility')
def conv (str):
    str = re.sub ('visibility-lambda', 'break-visibility', str)
    return str


@rule ((1, 5, 67), _ ('automaticMelismata turned on by default'))
def conv (str):
    if re.search (r'\addlyrics',str) \
           and re.search ('automaticMelismata', str)  == None:
        stderr_write (NOT_SMART % "automaticMelismata")
        stderr_write (_ ("automaticMelismata is turned on by default since 1.5.67."))
        stderr_write ('\n')
        raise FatalConversionError ()
    return str


@rule ((1, 5, 68), 'ly-set-X-property -> ly-set-X-property!')
def conv (str):
    str = re.sub ('ly-set-grob-property([^!])', 'ly-set-grob-property!\1', str)
    str = re.sub ('ly-set-mus-property([^!])', 'ly-set-mus-property!\1', str)
    return str


@rule ((1, 5, 71), 'extent-[XY] -> [XY]-extent')
def conv (str):
    str = re.sub ('extent-X', 'X-extent', str)
    str = re.sub ('extent-Y', 'Y-extent', str)
    return str


@rule ((1, 5, 72), 'set! point-and-click -> set-point-and-click!')
def conv (str):
    str = re.sub ("""#\(set! +point-and-click +line-column-location\)""",
                  """#(set-point-and-click! \'line-column)""", str)
    str = re.sub ("""#\(set![ \t]+point-and-click +line-location\)""",
                  '#(set-point-and-click! \'line)', str)
    str = re.sub ('#\(set! +point-and-click +#f\)',
                  '#(set-point-and-click! \'none)', str)
    return str


@rule ((1, 6, 5), 'Stems: flag-style -> stroke-style; style -> flag-style')
def conv (str):
    str = re.sub ('flag-style', 'stroke-style', str)
    str = re.sub (r"""Stem([ ]+)\\override #'style""", r"""Stem \\override #'flag-style""", str);
    str = re.sub (r"""Stem([ ]+)\\set([ ]+)#'style""", r"""Stem \\set #'flag-style""", str);
    return str


def subst_req_name (match):
    return "(make-music-by-name \'%sEvent)" % regularize_id (match.group(1))


@rule ((1, 7, 1), 'ly-make-music foo_bar_req -> make-music-by-name FooBarEvent')
def conv (str):
    str = re.sub ('\\(ly-make-music *\"([A-Z][a-z_]+)_req\"\\)', subst_req_name, str)
    str = re.sub ('Request_chord', 'EventChord', str)
    return str


spanner_subst ={
        "text" : 'TextSpanEvent',
        "decrescendo" : 'DecrescendoEvent',
        "crescendo" : 'CrescendoEvent',
        "Sustain" : 'SustainPedalEvent',
        "slur" : 'SlurEvent',
        "UnaCorda" : 'UnaCordaEvent',
        "Sostenuto" : 'SostenutoEvent',
        }

def subst_ev_name (match):
    stype = 'STOP'
    if re.search ('start', match.group(1)):
        stype= 'START'
    mtype = spanner_subst[match.group(2)]
    return "(make-span-event '%s %s)" % (mtype , stype)

def subst_definition_ev_name(match):
    return ' = #%s' % subst_ev_name (match)

def subst_inline_ev_name (match):
    s = subst_ev_name (match)
    return '#(ly-export %s)' % s

def subst_csp_definition (match):
    return ' = #(make-event-chord (list %s))' % subst_ev_name (match)

def subst_csp_inline (match):
    return '#(ly-export (make-event-chord (list %s)))' % subst_ev_name (match)


@rule ((1, 7, 2), '\\spanrequest -> #(make-span-event .. ), \script -> #(make-articulation .. )')
def conv (str):
    str = re.sub (r' *= *\\spanrequest *([^ ]+) *"([^"]+)"', subst_definition_ev_name, str)
    str = re.sub (r'\\spanrequest *([^ ]+) *"([^"]+)"', subst_inline_ev_name, str)
    str = re.sub (r' *= *\\commandspanrequest *([^ ]+) *"([^"]+)"', subst_csp_definition, str)
    str = re.sub (r'\\commandspanrequest *([^ ]+) *"([^"]+)"', subst_csp_inline, str)
    str = re.sub (r'ly-id ', 'ly-import ', str)

    str = re.sub (r' *= *\\script "([^"]+)"', ' = #(make-articulation "\\1")', str)
    str = re.sub (r'\\script "([^"]+)"', '#(ly-export (make-articulation "\\1"))', str)
    return str


@rule ((1, 7, 3), 'ly- -> ly:')
def conv(str):
    str = re.sub (r'\(ly-', '(ly:', str)

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

    origre = r'\b(%s)' % '|'.join (changed)

    str = re.sub (origre, r'ly:\1',str)
    str = re.sub ('set-point-and-click!', 'set-point-and-click', str)
    return str


@rule ((1, 7, 4), '<< >> -> < <  > >')
def conv(str):
    if re.search ('new-chords-done',str):
        return str

    str = re.sub (r'<<', '< <', str)
    str = re.sub (r'>>', '> >', str)
    return str


@rule ((1, 7, 5), '\\transpose TO -> \\transpose FROM  TO')
def conv(str):
    str = re.sub (r"\\transpose", r"\\transpose c'", str)
    str = re.sub (r"\\transpose c' *([a-z]+)'", r"\\transpose c \1", str)
    return str


@rule ((1, 7, 6), 'note\\script -> note-\script')
def conv(str):
    kws =   ['arpeggio',
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

    origstr = '|'.join (kws)
    str = re.sub (r'([^_^-])\\(%s)\b' % origstr, r'\1-\\\2', str)
    return str


@rule ((1, 7, 10), "\property ChordName #'style -> #(set-chord-name-style 'style)")
def conv(str):
    str = re.sub (r"\\property *ChordNames *\. *ChordName *\\(set|override) *#'style *= *#('[a-z]+)",
                  r"#(set-chord-name-style \2)", str)
    str = re.sub (r"\\property *ChordNames *\. *ChordName *\\revert *#'style",
                  r"", str)
    return str


@rule ((1, 7, 11), "transpose-pitch -> pitch-transpose")
def conv(str):
    str = re.sub (r"ly:transpose-pitch", "ly:pitch-transpose", str)
    return str


@rule ((1, 7, 13), "ly:XX-molecule-YY -> ly:molecule-XX-YY")
def conv(str):
    str = re.sub (r"ly:get-molecule-extent", "ly:molecule-get-extent", str)
    str = re.sub (r"ly:set-molecule-extent!", "ly:molecule-set-extent!", str)
    str = re.sub (r"ly:add-molecule", "ly:molecule-add", str)
    str = re.sub (r"ly:combine-molecule-at-edge", "ly:molecule-combine-at-edge", str)
    str = re.sub (r"ly:align-to!", "ly:molecule-align-to!", str)
    return str


@rule ((1, 7, 15), "linewidth = -1 -> raggedright = ##t")
def conv(str):
    str = re.sub (r"linewidth *= *-[0-9.]+ *(\\mm|\\cm|\\in|\\pt)?", 'raggedright = ##t', str )
    return str


@rule ((1, 7, 16), "divisiomaior -> divisioMaior")
def conv(str):
    str = re.sub ("divisiomaior",
                  "divisioMaior", str)
    str = re.sub ("divisiominima",
                  "divisioMinima", str)
    str = re.sub ("divisiomaxima",
                  "divisioMaxima", str)
    return str


@rule ((1, 7, 17), "Skip_req  -> Skip_event")
def conv(str):
    str = re.sub ("Skip_req_swallow_translator",
                  "Skip_event_swallow_translator", str)
    return str


@rule ((1, 7, 18), "groupOpen/Close  -> start/stopGroup, #'outer  -> #'enclose-bounds")
def conv(str):
    str = re.sub ("groupOpen",
                  "startGroup", str)
    str = re.sub ("groupClose",
                  "stopGroup", str)
    str = re.sub ("#'outer",
                  "#'enclose-bounds", str)

    return str


@rule ((1, 7, 19), _ ("remove %s") % "GraceContext")
def conv(str):
    if re.search( r'\\GraceContext', str):
        stderr_write (NOT_SMART % "GraceContext")
        stderr_write (FROM_TO \
                          % ("GraceContext", "#(add-to-grace-init .. )"))
        stderr_write (UPDATE_MANUALLY)
        raise FatalConversionError ()

    str = re.sub ('HaraKiriStaffContext', 'RemoveEmptyStaffContext', str)
    return str


@rule ((1, 7, 22), "#'type -> #'style")
def conv(str):
    str = re.sub (
            r"(set|override|revert) *#'type",
            r"\1 #'style",
            str)
    return str


@rule ((1, 7, 23), "barNonAuto -> automaticBars")
def conv(str):
    str = re.sub (
            "barNonAuto *= *##t",
            "automaticBars = ##f",
            str)
    str = re.sub (
            "barNonAuto *= *##f",
            "automaticBars = ##t",
            str)
    return str


@rule ((1, 7, 24), _ ("cluster syntax"))
def conv(str):
    if re.search( r'-(start|stop)Cluster', str):
        stderr_write (NOT_SMART % _ ("cluster syntax"))
        stderr_write (UPDATE_MANUALLY)

        raise FatalConversionError ()
    return str


@rule ((1, 7, 28), _ ("new Pedal style syntax"))
def conv(str):
    str = re.sub (r"\\property *Staff\.(Sustain|Sostenuto|UnaCorda)Pedal *\\(override|set) *#'pedal-type *",
                    r"\property Staff.pedal\1Style ", str)
    str = re.sub (r"\\property *Staff\.(Sustain|Sostenuto|UnaCorda)Pedal *\\revert *#'pedal-type", '', str)
    return str


def sub_chord (m):
    str = m.group(1)

    origstr =  '<%s>' % str
    if re.search (r'\\\\', str):
        return origstr

    if re.search (r'\\property', str):
        return origstr

    if re.match (r'^\s*\)?\s*\\[a-zA-Z]+', str):
        return origstr

    durs = []
    def sub_durs (m, durs = durs):
        durs.append(m.group(2))
        return m.group (1)

    str = re.sub (r"([a-z]+[,'!? ]*)([0-9]+\.*)", sub_durs, str)
    dur_str = ''

    for d in durs:
        if dur_str == '':
            dur_str = d
        if dur_str <> d:
            return '<%s>' % m.group (1)

    pslur_strs = ['']
    dyns = ['']
    slur_strs = ['']

    last_str = ''
    while last_str <> str:
        last_str = str

        def sub_tremolos (m, slur_strs = slur_strs):
            tr = m.group (2)
            if tr not in slur_strs:
                slur_strs.append (tr)
            return  m.group (1)

        str = re.sub (r"([a-z]+[',!? ]*)(:[0-9]+)",
                      sub_tremolos, str)

        def sub_dyn_end (m, dyns = dyns):
            dyns.append (' \!')
            return ' ' + m.group(2)

        str = re.sub (r'(\\!)\s*([a-z]+)', sub_dyn_end, str)
        def sub_slurs(m, slur_strs = slur_strs):
            if '-)' not in slur_strs:
                slur_strs.append (')')
            return m.group(1)

        def sub_p_slurs(m, slur_strs = slur_strs):
            if '-\)' not in slur_strs:
                slur_strs.append ('\)')
            return m.group(1)

        str = re.sub (r"\)[ ]*([a-z]+)", sub_slurs, str)
        str = re.sub (r"\\\)[ ]*([a-z]+)", sub_p_slurs, str)
        def sub_begin_slurs(m, slur_strs = slur_strs):
            if '-(' not in slur_strs:
                slur_strs.append ('(')
            return m.group(1)

        str = re.sub (r"([a-z]+[,'!?0-9 ]*)\(",
                      sub_begin_slurs, str)
        def sub_begin_p_slurs(m, slur_strs = slur_strs):
            if '-\(' not in slur_strs:
                slur_strs.append ('\(')
            return m.group(1)

        str = re.sub (r"([a-z]+[,'!?0-9 ]*)\\\(",
                sub_begin_p_slurs, str)

        def sub_dyns (m, slur_strs = slur_strs):
            s = m.group(0)
            if s == '@STARTCRESC@':
                slur_strs.append ("\\<")
            elif s == '@STARTDECRESC@':
                slur_strs.append ("\\>")
            elif s == r'-?\\!':
                slur_strs.append ('\\!')
            return ''

        str = re.sub (r'@STARTCRESC@', sub_dyns, str)
        str = re.sub (r'-?\\!', sub_dyns, str)

        def sub_articulations (m, slur_strs = slur_strs):
            a = m.group(1)
            if a not in slur_strs:
                slur_strs.append (a)
            return ''

        str = re.sub (r"([_^-]\@ACCENT\@)", sub_articulations,
                      str)
        str = re.sub (r"([_^-]\\[a-z]+)", sub_articulations,
                      str)
        str = re.sub (r"([_^-][>_.+|^-])", sub_articulations,
                      str)
        str = re.sub (r'([_^-]"[^"]+")', sub_articulations,
                      str)

        def sub_pslurs(m, slur_strs = slur_strs):
            slur_strs.append (' \\)')
            return m.group(1)
        str = re.sub (r"\\\)[ ]*([a-z]+)", sub_pslurs, str)

    ## end of while <>

    suffix = ''.join (slur_strs) + ''.join (pslur_strs) \
             + ''.join (dyns)

    return '@STARTCHORD@%s@ENDCHORD@%s%s' % (str , dur_str, suffix)



def sub_chords (str):
    simend = '>'
    simstart = '<'
    chordstart = '<<'
    chordend = '>>'
    marker_str = '%% new-chords-done %%'

    if re.search (marker_str,str):
        return str
    str = re.sub ('<<', '@STARTCHORD@', str)
    str = re.sub ('>>', '@ENDCHORD@', str)

    str = re.sub (r'\\<', '@STARTCRESC@', str)
    str = re.sub (r'\\>', '@STARTDECRESC@', str)
    str = re.sub (r'([_^-])>', r'\1@ACCENT@', str)
    str = re.sub (r'<([^<>{}]+)>', sub_chord, str)

    # add dash: -[, so that [<<a b>> c d] becomes
    #                      <<a b>>-[ c d]
    # and gets skipped by articulation_substitute
    str = re.sub (r'\[ *(@STARTCHORD@[^@]+@ENDCHORD@[0-9.]*)',
                  r'\1-[', str)
    str = re.sub (r'\\! *(@STARTCHORD@[^@]+@ENDCHORD@[0-9.]*)',
                  r'\1-\\!', str)

    str = re.sub (r'<([^?])', r'%s\1' % simstart, str)
    str = re.sub (r'>([^?])', r'%s\1' % simend,  str)
    str = re.sub ('@STARTCRESC@', r'\\<', str)
    str = re.sub ('@STARTDECRESC@', r'\\>' ,str)
    str = re.sub (r'\\context *Voice *@STARTCHORD@',
                  '@STARTCHORD@', str)
    str = re.sub ('@STARTCHORD@', chordstart, str)
    str = re.sub ('@ENDCHORD@', chordend, str)
    str = re.sub (r'@ACCENT@', '>', str)
    return str

markup_start = re.compile(r"([-^_]|\\mark)\s*(#\s*'\s*)\(")
musicglyph = re.compile(r"\(\s*music\b")
columns = re.compile(r"\(\s*columns\b")
submarkup_start = re.compile(r"\(\s*([a-zA-Z]+)")
leftpar = re.compile(r"\(")
rightpar = re.compile(r"\)")

def text_markup (str):
    result = ''
    # Find the beginning of each markup:
    match = markup_start.search (str)
    while match:
        result = result + str[:match.end (1)] + " \markup"
        str = str[match.end( 2):]
        # Count matching parentheses to find the end of the
        # current markup:
        nesting_level = 0
        pars = re.finditer(r"[()]",str)
        for par in pars:
            if par.group () == '(':
                nesting_level = nesting_level + 1
            else:
                nesting_level = nesting_level - 1
            if nesting_level == 0:
                markup_end = par.end ()
                break
        # The full markup in old syntax:
        markup = str[:markup_end]
        # Modify to new syntax:
        markup = musicglyph.sub (r"{\\musicglyph", markup)
        markup = columns.sub (r"{", markup)
        markup = submarkup_start.sub (r"{\\\1", markup)
        markup = leftpar.sub ("{", markup)
        markup = rightpar.sub ("}", markup)

        result = result + markup
        # Find next markup
        str = str[markup_end:]
        match = markup_start.search(str)
    result = result + str
    return result

def articulation_substitute (str):
    str = re.sub (r"""([^-])\[ *(\\?\)?[a-z]+[,']*[!?]?[0-9:]*\.*)""",
                  r"\1 \2[", str)
    str = re.sub (r"""([^-])\\\) *([a-z]+[,']*[!?]?[0-9:]*\.*)""",
                  r"\1 \2\\)", str)
    str = re.sub (r"""([^-\\])\) *([a-z]+[,']*[!?]?[0-9:]*\.*)""",
                  r"\1 \2)", str)
    str = re.sub (r"""([^-])\\! *([a-z]+[,']*[!?]?[0-9:]*\.*)""",
                  r"\1 \2\\!", str)
    return str

string_or_scheme = re.compile ('("(?:[^"\\\\]|\\\\.)*")|(#\\s*\'?\\s*\\()')

# Only apply articulation_substitute () outside strings and
# Scheme expressions:
def smarter_articulation_subst (str):
    result = ''
    # Find the beginning of next string or Scheme expr.:
    match = string_or_scheme.search (str)
    while match:
        # Convert the preceding LilyPond code:
        previous_chunk = str[:match.start()]
        result = result + articulation_substitute (previous_chunk)
        if match.group (1): # Found a string
            # Copy the string to output:
            result = result + match.group (1)
            str = str[match.end(1):]
        else: # Found a Scheme expression. Count
            # matching parentheses to find its end
            str = str[match.start ():]
            nesting_level = 0
            pars = re.finditer(r"[()]",str)
            for par in pars:
                if par.group () == '(':
                    nesting_level = nesting_level + 1
                else:
                    nesting_level = nesting_level - 1
                if nesting_level == 0:
                    scheme_end = par.end ()
                    break
            # Copy the Scheme expression to output:
            result = result + str[:scheme_end]
            str = str[scheme_end:]
        # Find next string or Scheme expression:
        match = string_or_scheme.search (str)
    # Convert the remainder of the file
    result = result + articulation_substitute (str)
    return result

def conv_relative(str):
    if re.search (r"\\relative", str):
        str= "#(ly:set-option 'old-relative)\n" + str

    return str

@rule ((1, 9, 0), _ ("""New relative mode,
Postfix articulations, new text markup syntax, new chord syntax."""))
def conv (str):
    str = re.sub (r"#'\(\)", "@SCM_EOL@", str)
    str =  conv_relative (str)
    str = sub_chords (str)

    str = text_markup (str)
    str = smarter_articulation_subst (str)
    str = re.sub ("@SCM_EOL@", "#'()", str)
    return str


@rule ((1, 9, 1), _ ("Remove - before articulation"))
def conv (str):
    if re.search ("font-style",str):
        stderr_write (NOT_SMART % "font-style")
        stderr_write (UPDATE_MANUALLY)

        raise FatalConversionError ()

    str = re.sub (r'-\\markup', r'@\\markup', str)
    str = re.sub (r'-\\', r'\\', str)
    str = re.sub (r'-\)', ')', str)
    str = re.sub (r'-\(', '(', str)
    str = re.sub ('-\[', '[', str)
    str = re.sub ('-\]', ']', str)
    str = re.sub ('-~', '~', str)
    str = re.sub (r'@\\markup', r'-\\markup', str)
    return str


@rule ((1, 9, 2), "\\newcontext -> \\new")
def conv (str):
    str = re.sub ('ly:set-context-property',
                  'ly:set-context-property!', str)
    str = re.sub ('\\\\newcontext', '\\\\new', str)
    str = re.sub ('\\\\grace[\t\n ]*([^{ ]+)',
                  r'\\grace { \1 }', str)
    str = re.sub ("\\\\grace[\t\n ]*{([^}]+)}",
                  r"""\\grace {
\\property Voice.Stem \\override #'stroke-style = #"grace"
  \1
  \\property Voice.Stem \\revert #'stroke-style }
""", str)
    return str


@rule ((1, 9, 3), (_ ("%s misspelling") % "\\acciaccatura") +
                         ", fingerHorizontalDirection -> fingeringOrientations")
def conv (str):
    str = re.sub ('accacciatura',
                  'acciaccatura', str)

    if re.search ("context-spec-music", str):
        stderr_write (NOT_SMART % "context-spec-music")
        stderr_write (UPDATE_MANUALLY)

        raise FatalConversionError ()

    str = re.sub ('fingerHorizontalDirection *= *#(LEFT|-1)',
                  "fingeringOrientations = #'(up down left)", str)
    str = re.sub ('fingerHorizontalDirection *= *#(RIGHT|1)',
                  "fingeringOrientations = #'(up down right)", str)
    return str


@rule ((1, 9, 4), _ ('Swap < > and << >>'))
def conv (str):
    if re.search ('\\figures', str):
        warning (_ ("attempting automatic \\figures conversion.  Check results!"));

    def figures_replace (m):
        s = m.group (1)
        s = re.sub ('<', '@FIGOPEN@',s)
        s = re.sub ('>', '@FIGCLOSE@',s)
        return '\\figures { %s }' % s

    str = re.sub (r'\\figures[ \t\n]*{([^}]+)}', figures_replace, str)
    str = re.sub (r'\\<', '@STARTCRESC@', str)
    str = re.sub (r'\\>', '@STARTDECRESC@', str)
    str = re.sub (r'([-^_])>', r'\1@ACCENT@', str)
    str = re.sub (r'<<', '@STARTCHORD@', str)
    str = re.sub (r'>>', '@ENDCHORD@', str)
    str = re.sub (r'>', '@ENDSIMUL@', str)
    str = re.sub (r'<', '@STARTSIMUL@', str)
    str = re.sub ('@STARTDECRESC@', '\\>', str)
    str = re.sub ('@STARTCRESC@', '\\<', str)
    str = re.sub ('@ACCENT@', '>', str)
    str = re.sub ('@ENDCHORD@', '>', str)
    str = re.sub ('@STARTCHORD@', '<', str)
    str = re.sub ('@STARTSIMUL@', '<<', str)
    str = re.sub ('@ENDSIMUL@', '>>', str)
    str = re.sub ('@FIGOPEN@', '<', str)
    str = re.sub ('@FIGCLOSE@', '>', str)
    return str


@rule ((1, 9, 5), 'HaraKiriVerticalGroup -> RemoveEmptyVerticalGroup')
def conv (str):
    str = re.sub ('HaraKiriVerticalGroup', 'RemoveEmptyVerticalGroup', str)
    return str


@rule ((1, 9, 6), _ ('deprecate %s') % 'ly:get-font')
def conv (str):
    if re.search ("ly:get-font", str) :
        stderr_write (NOT_SMART % "ly:get-font")
        stderr_write (FROM_TO \
                          % ("(ly:paper-get-font (ly:grob-get-paper foo) .. )",
                             "(ly:paper-get-font (ly:grob-get-paper foo) .. )"))
        stderr_write (UPDATE_MANUALLY)
        raise FatalConversionError ()

    if re.search ("\\pitch *#", str) :
        stderr_write (NOT_SMART % "\\pitch")
        stderr_write (_ ("Use Scheme code to construct arbitrary note events."))
        stderr_write ('\n')

        raise FatalConversionError ()
    return str


@rule ((1, 9, 7), _ ('''use symbolic constants for alterations,
remove \\outputproperty, move ly:verbose into ly:get-option'''))
def conv (str):
    def sub_alteration (m):
        alt = m.group (3)
        alt = {
                '-1': 'FLAT',
                '-2': 'DOUBLE-FLAT',
                '0': 'NATURAL',
                '1': 'SHARP',
                '2': 'DOUBLE-SHARP',
                }[alt]

        return '(ly:make-pitch %s %s %s)' % (m.group(1), m.group (2),
                                             alt)

    str =re.sub ("\\(ly:make-pitch *([0-9-]+) *([0-9-]+) *([0-9-]+) *\\)",
                 sub_alteration, str)


    str = re.sub ("ly:verbose", "ly:get-option 'verbose", str)

    m= re.search ("\\\\outputproperty #([^#]+)[\t\n ]*#'([^ ]+)", str)
    if m:
        stderr_write (_ (\
                r"""\outputproperty found,
Please hand-edit, using

  \applyoutput #(outputproperty-compatibility %s '%s <GROB PROPERTY VALUE>)

as a substitution text.""") % (m.group (1), m.group (2)) )
        raise FatalConversionError ()

    if re.search ("ly:(make-pitch|pitch-alteration)", str) \
           or re.search ("keySignature", str):
        stderr_write (NOT_SMART % "pitches")
        stderr_write (
            _ ("""The alteration field of Scheme pitches was multiplied by 2
to support quarter tone accidentals.  You must update the following constructs manually:

* calls of ly:make-pitch and ly:pitch-alteration
* keySignature settings made with \property
"""))
        raise FatalConversionError ()
    return str


@rule ((1, 9, 8), "dash-length -> dash-fraction")
def conv (str):
    if re.search ("dash-length",str):
        stderr_write (NOT_SMART % "dash-length")
        stderr_write (FROM_TO % ("dash-length", "dash-fraction"))
        stderr_write (UPDATE_MANUALLY)
        raise FatalConversionError ()
    return str


@rule ((2, 1, 1), "font-relative-size -> font-size")
def conv (str):
    def func(match):
        return "#'font-size = #%d" % (2*int (match.group (1)))

    str =re.sub (r"#'font-relative-size\s*=\s*#\+?([0-9-]+)", func, str)
    str =re.sub (r"#'font-family\s*=\s*#'ancient",
                 r"#'font-family = #'music", str)
    return str


@rule ((2, 1, 2), "ly:get-music-length -> ly:music-length")
def conv (str):
    str =re.sub (r"ly:get-music-length", "ly:music-length", str)
    return str


@rule ((2, 1, 3), "stanza -> instrument")
def conv (str):
    str =re.sub (r"\.\s+stz=", ". instr ", str)
    return str


@rule ((2, 1, 4), _ ("removal of automaticMelismata; use melismaBusyProperties instead."))
def conv (str):
    def func (match):
        c = match.group (1)
        b = match.group (2)

        if b == 't':
            if c == 'Score':
                return ''
            else:
                return r" \property %s.melismaBusyProperties \unset"  % c
        elif b == 'f':
            return r"\property %s.melismaBusyProperties = #'(melismaBusy)"  % c

    str = re.sub (r"\\property ([a-zA-Z]+)\s*\.\s*automaticMelismata\s*=\s*##([ft])", func, str)
    return str


@rule ((2, 1, 7), "\\translator Staff -> \\change Staff")
def conv (str):
    str =re.sub (r"\\translator\s+([a-zA-Z]+)", r"\\change \1", str)
    return str


@rule ((2, 1, 10), "\\newaddlyrics -> \\lyricsto")
def conv (str):
    str =re.sub (r"\\newaddlyrics", r"\\lyricsto", str)
    return str


@rule ((2, 1, 11), """\\include "paper16.ly" -> #(set-staff-size 16)
\\note #3 #1 #1 -> \\note #"8." #1
""")
def conv (str):
    str = re.sub (r'\\include\s*"paper([0-9]+)(-init)?.ly"',
                  r"#(set-staff-size \1)", str)

    def sub_note (match):
        dur = ''
        log = int (match.group (1))
        dots = int (match.group (2))

        if log >= 0:
            dur = '%d' % (1 << log)
        else:
            dur = { -1 : 'breve',
                    -2 : 'longa',
                    -3 : 'maxima'}[log]

        dur += ('.' * dots)

        return r'\note #"%s" #%s' % (dur, match.group (3))

    str = re.sub (r'\\note\s+#([0-9-]+)\s+#([0-9]+)\s+#([0-9.-]+)',
                  sub_note, str)
    return str


@rule ((2, 1, 12), "OttavaSpanner -> OttavaBracket")
def conv (str):
    str = re.sub (r"OttavaSpanner", r"OttavaBracket", str)
    return str


@rule ((2, 1, 13), "set-staff-size -> set-global-staff-size")
def conv (str):
    str = re.sub (r"\(set-staff-size ", r"(set-global-staff-size ", str)
    return str


@rule ((2, 1, 14), "style = dotted -> dash-fraction = 0")
def conv (str):
    str = re.sub (r"#'style\s*=\s*#'dotted-line",
                 r"#'dash-fraction = #0.0 ", str)
    return str


@rule ((2, 1, 15), "LyricsVoice . instr(ument) -> vocalName")
def conv (str):
    str = re.sub (r'LyricsVoice\s*\.\s*instrument\s*=\s*("[^"]*")',
                 r'LyricsVoice . vocalName = \1', str)

    str = re.sub (r'LyricsVoice\s*\.\s*instr\s*=\s*("[^"]*")',
                 r'LyricsVoice . vocNam = \1', str)
    return str


@rule ((2, 1, 16), '\\musicglyph #"accidentals-NUM" -> \\sharp/flat/etc.')
def conv (str):
    def sub_acc (m):
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
        return '\\%s' %  d[m.group (1)]

    str = re.sub (r'\\musicglyph\s*#"accidentals-([0-9-]+)"',
                  sub_acc, str)
    return str


@rule ((2, 1, 17), _ ("\\partcombine syntax change to \\newpartcombine"))
def conv (str):

    if re.search (r'\\partcombine', str):
        stderr_write (NOT_SMART % "\\partcombine")
        stderr_write (UPDATE_MANUALLY)
        raise FatalConversionError ()

    # this rule doesn't really work,
    # too lazy to figure out why.
    str = re.sub (r'\\context\s+Voice\s*=\s*one\s*\\partcombine\s+Voice\s*\\context\s+Thread\s*=\s*one(.*)\s*'
                  + r'\\context\s+Thread\s*=\s*two',
                  '\\\\newpartcombine\n\\1\n', str)
    return str


@rule ((2, 1, 18), """\\newpartcombine -> \\partcombine,
\\autochange Staff -> \\autochange
""")
def conv (str):
    str = re.sub (r'\\newpartcombine', r'\\partcombine', str)
    str = re.sub (r'\\autochange\s+Staff', r'\\autochange ', str)
    return str


@rule ((2, 1, 19), _ ("""Drum notation changes, Removing \\chordmodifiers, \\notenames.
Harmonic notes. Thread context removed. Lyrics context removed."""))
def conv (str):
    if re.search ('include "drumpitch', str):
        stderr_write (_ ("Drums found. Enclose drum notes in \\drummode"))

    str = re.sub (r'\\include "drumpitch-init.ly"','', str)

    str = re.sub (r'\\pitchnames ','pitchnames = ', str)
    str = re.sub (r'\\chordmodifiers ','chordmodifiers = ', str)
    str = re.sub (r'\bdrums\b\s*=','drumContents = ', str)
    str = re.sub (r'\\drums\b','\\drumContents ', str)


    if re.search ('drums->paper', str):
        stderr_write (_ ("\n%s found. Check file manually!\n") % _("Drum notation"))

    str = re.sub (r"""\\apply\s+#\(drums->paper\s+'([a-z]+)\)""",
                  r"""\property DrumStaff.drumStyleTable = #\1-style""",
                  str)

    if re.search ('Thread', str):
        stderr_write (_ ("\n%s found. Check file manually!\n") % "Thread");

    str = re.sub (r"""(\\once\s*)?\\property\s+Thread\s*\.\s*NoteHead\s*"""
                  + r"""\\(set|override)\s*#'style\s*=\s*#'harmonic"""
                  + r"""\s+([a-z]+[,'=]*)([0-9]*\.*)"""
                  ,r"""<\3\\harmonic>\4""", str)

    str = re.sub (r"""\\new Thread""", """\context Voice""", str)
    str = re.sub (r"""Thread""", """Voice""", str)

    if re.search ('\bLyrics\b', str):
        stderr_write (_ ("\n%s found. Check file manually!\n") % "Lyrics");

    str = re.sub (r"""LyricsVoice""", r"""L@ricsVoice""", str)
    str = re.sub (r"""\bLyrics\b""", r"""LyricsVoice""", str)
    str = re.sub (r"""LyricsContext""", r"""LyricsVoiceContext""", str)
    str = re.sub (r"""L@ricsVoice""", r"""LyricsVoice""",str)
    return str


@rule ((2, 1, 20), "nonevent-skip -> skip-music")
def conv (str):
    str = re.sub (r'nonevent-skip', 'skip-music', str)
    return str


@rule ((2, 1, 21), """molecule-callback -> print-function,
brew_molecule -> print
brew-new-markup-molecule -> Text_item::print
LyricsVoice -> Lyrics
tupletInvisible -> TupletBracket \set #'transparent
%s.
""" % (_ ("remove %s") % "Grob::preset_extent"))
def conv (str):
    str = re.sub (r'molecule-callback', 'print-function', str)
    str = re.sub (r'brew_molecule', 'print', str)
    str = re.sub (r'brew-new-markup-molecule', 'Text_item::print', str)
    str = re.sub (r'LyricsVoice', 'Lyrics', str)
    str = re.sub (r'tupletInvisible',
                  r"TupletBracket \\set #'transparent", str)
#       str = re.sub (r'molecule', 'collage', str)
#molecule -> collage
    str = re.sub (r"\\property\s+[a-zA-Z]+\s*\.\s*[a-zA-Z]+\s*"
                  + r"\\set\s*#'X-extent-callback\s*=\s*#Grob::preset_extent",
                  "", str)
    return str


@rule ((2, 1, 22), """%s
        \\set A.B = #C , \\unset A.B
        \\override A.B #C = #D, \\revert A.B #C

""" % _ ("new syntax for property settings:"))
def conv (str):
    str = re.sub (r'(\\property[^=]+)=\s*([-0-9]+)',
                  r'\1= #\2', str)
    str = re.sub (r'\\property\s+([^. ]+)\s*\.\s*([^\\=]+)\s*\\(set|override)',
                  r"\\overrid@ \1.\2 ", str)
    str = re.sub (r'\\property\s+([^. ]+)\s*\.\s*([^\\= ]+)\s*=\s*',
                  r'\\s@t \1.\2 = ', str)
    str = re.sub (r'\\property\s+([^. ]+)\s*\.\s*([^\\= ]+)\s*\\unset',
                  r'\\uns@t \1.\2 ', str)
    str = re.sub (r'\\property\s+([^. ]+)\s*\.\s*([^\\= ]+)\s*\\revert'
                  + r"\s*#'([-a-z0-9_]+)",
                  r"\\rev@rt \1.\2 #'\3", str)
    str = re.sub (r'Voice\.', '', str)
    str = re.sub (r'Lyrics\.', '', str)
    str = re.sub (r'ChordNames\.', '', str)

    str = re.sub ('rev@rt', 'revert',str)
    str = re.sub ('s@t', 'set',str)
    str = re.sub ('overrid@', 'override',str)

    str = re.sub ('molecule', 'stencil', str)
    str = re.sub ('Molecule', 'Stencil', str)
    return str


@rule ((2, 1, 23), _ ("Property setting syntax in \\translator{ }"))
def conv (str):
    def subst_in_trans (match):
        s = match.group (0)
        s = re.sub (r'\s([a-zA-Z]+)\s*\\override',
                      r' \\override \1', s)
        s = re.sub (r'\s([a-zA-Z]+)\s*\\set',
                      r' \\override \1', s)
        s = re.sub (r'\s([a-zA-Z]+)\s*\\revert',
                      r' \\revert \1', s)
        return s
    str = re.sub (r'\\(translator|with)\s*{[^}]+}',  subst_in_trans, str)

    def sub_abs (m):

        context = m.group ('context')
        d = m.groupdict ()
        if context:
            context = " '%s" % context[:-1] # -1: remove .
        else:
            context = ''

        d['context'] = context

        return r"""#(override-auto-beam-setting %(prop)s %(num)s %(den)s%(context)s)""" % d

    str = re.sub (r"""\\override\s*(?P<context>[a-zA-Z]+\s*\.\s*)?autoBeamSettings"""
                  +r"""\s*#(?P<prop>[^=]+)\s*=\s*#\(ly:make-moment\s+(?P<num>\d+)\s+(?P<den>\d)\s*\)""",
                  sub_abs, str)
    return str


@rule ((2, 1, 24), "music-list? -> ly:music-list?")
def conv (str):
    str = re.sub (r'music-list\?', 'ly:music-list?', str)
    str = re.sub (r'\|\s*~', '~ |', str)
    return str


@rule ((2, 1, 25), _ ("Scheme grob function renaming"))
def conv (str):
    str = re.sub (r'ly:get-spanner-bound', 'ly:spanner-get-bound', str)
    str = re.sub (r'ly:get-extent', 'ly:grob-extent', str)
    str = re.sub (r'ly:get-system', 'ly:grob-system', str)
    str = re.sub (r'ly:get-original', 'ly:grob-original', str)
    str = re.sub (r'ly:get-parent', 'ly:grob-parent', str)
    str = re.sub (r'ly:get-broken-into', 'ly:spanner-broken-into', str)
    str = re.sub (r'Melisma_engraver', 'Melisma_translator', str)
    if re.search ("ly:get-paper-variable", str):
        stderr_write (NOT_SMART % "ly:paper-get-variable")
        stderr_write (_ ('Use %s\n') % '(ly:paper-lookup (ly:grob-paper ))')
        raise FatalConversionError ()

    str = re.sub (r'\\defaultAccidentals', "#(set-accidental-style 'default)", str)
    str = re.sub (r'\\voiceAccidentals', "#(set-accidental-style 'voice)", str)
    str = re.sub (r'\\modernAccidentals', "#(set-accidental-style 'modern)", str)
    str = re.sub (r'\\modernCautionaries', "#(set-accidental-style 'modern-cautionary)", str)
    str = re.sub (r'\\modernVoiceAccidental', "#(set-accidental-style 'modern-voice)", str)
    str = re.sub (r'\\modernVoiceCautionaries', "#(set-accidental-style 'modern-voice-cautionary)", str)
    str = re.sub (r'\\pianoAccidentals', "#(set-accidental-style 'piano)", str)
    str = re.sub (r'\\pianoCautionaries', "#(set-accidental-style 'piano-cautionary)", str)
    str = re.sub (r'\\forgetAccidentals', "#(set-accidental-style 'forget)", str)
    str = re.sub (r'\\noResetKey', "#(set-accidental-style 'no-reset)", str)
    return str


@rule ((2, 1, 26), _ ("More Scheme function renaming"))
def conv (str):
    str = re.sub ('ly:set-grob-property!', 'ly:grob-set-property!',str)
    str = re.sub ('ly:set-mus-property!', 'ly:music-set-property!',str)
    str = re.sub ('ly:set-context-property!', 'ly:context-set-property!', str)
    str = re.sub ('ly:get-grob-property', 'ly:grob-property',str)
    str = re.sub ('ly:get-mus-property', 'ly:music-property',str)
    str = re.sub ('ly:get-context-property', 'ly:context-property',str)
    return str


@rule ((2, 1, 27), "property transposing -> tuning")
def conv (str):
    def subst (m):
        g = int (m.group (2))
        o = g / 12
        g -= o * 12
        if g <  0:
            g += 12
            o -= 1


        lower_pitches = filter (lambda x : x <= g, [0, 2, 4, 5, 7, 9, 11, 12])
        s = len (lower_pitches) -1
        a = g - lower_pitches [-1]


        str = 'cdefgab' [s]
        str += ['eses', 'es', '', 'is', 'isis'][a + 2]
        if o < 0:
            str += ',' * (-o - 1)
        elif o >= 0:
            str += "'" * (o + 1)

        return '\\transposition %s ' % str


    str = re.sub (r"\\set ([A-Za-z]+\s*\.\s*)?transposing\s*=\s*#([-0-9]+)",
                  subst, str)
    return str


@rule ((2, 1, 28), """make-music-by-name -> make-music,
new syntax for setting \\arpeggioBracket""")
def conv (str):
    str = re.sub (r'make-music-by-name', 'make-music', str)
    str = re.sub (r"\\override\s+.*Arpeggio\s+#.print-function\s+=\s+\\arpeggioBracket", r"\\arpeggioBracket", str)
    return str


@rule ((2, 1, 29), '\\center -> \\center-align, \\translator -> \\context')
def conv (str):
    str = re.sub (r'\\center([^-])', '\\center-align\\1', str)
    str = re.sub (r'\\translator', '\\context', str)
    return str


@rule ((2, 1, 30), '''\\threeq{flat,sharp} -> \\sesqui{flat,sharp}
ly:get-mutable-properties -> ly:mutable-music-properties
centralCPosition -> middleCPosition
ly:unset-context-property -> ly:context-unset-property
ly:translator-find -> ly:context-find
ly:get-stencil-extent -> ly:stencil-extent
''')
def conv (str):
    str = re.sub (r'\\threeq(flat|sharp)', r'\\sesqui\1', str)
    str = re.sub (r'ly:stencil-get-extent',
                  'ly:stencil-extent', str)
    str = re.sub (r'ly:translator-find',
                  'ly:context-find', str)
    str = re.sub ('ly:unset-context-property','ly:context-unset-property',
                  str)

    str = re.sub (r'ly:get-mutable-properties',
                  'ly:mutable-music-properties',str)
    str = re.sub (r'centralCPosition',
                  'middleCPosition',str)
    return str


@rule ((2, 1, 31), 'remove \\alias Timing')
def conv (str):
    str = re.sub (r'\\alias\s*"?Timing"?', '', str)
    return str


@rule ((2, 1, 33), 'breakAlignOrder -> break-align-orders.')
def conv (str):
    str = re.sub (r"(\\set\s+)?(?P<context>(Score\.)?)breakAlignOrder\s*=\s*#'(?P<list>[^\)]+)",
                  r"\n\\override \g<context>BreakAlignment #'break-align-orders = "
                  + "#(make-vector 3 '\g<list>)", str)
    return str


@rule ((2, 1, 34), 'set-paper-size -> set-default-paper-size.')
def conv (str):
    str = re.sub (r"\(set-paper-size",
                  "(set-default-paper-size",str)
    return str


@rule ((2, 1, 36), 'ly:mutable-music-properties -> ly:music-mutable-properties')
def conv (str):
    str = re.sub (r"ly:mutable-music-properties",
                  "ly:music-mutable-properties", str)
    return str


@rule ((2, 2, 0), _ ("bump version for release"))
def conv (str):
    return str


@rule ((2, 3, 1), '\\apply -> \\applymusic')
def conv (str):
    return re.sub (r'\\apply\b', r'\\applymusic', str)


@rule ((2, 3, 2), '\\FooContext -> \\Foo')
def conv (str):
    if re.search ('textheight', str):
        stderr_write (NOT_SMART % "textheight")
        stderr_write (UPDATE_MANUALLY)
        stderr_write (
_ ("""Page layout has been changed, using paper size and margins.
textheight is no longer used.
"""))
    str = re.sub (r'\\OrchestralScoreContext', '\\Score', str)
    def func(m):
        if m.group(1) not in ['RemoveEmptyStaff',
                              'AncientRemoveEmptyStaffContext',
                              'EasyNotation']:
            return '\\' + m.group (1)
        else:
            return m.group (0)


    str = re.sub (r'\\([a-zA-Z]+)Context\b', func, str)
    str = re.sub ('ly:paper-lookup', 'ly:output-def-lookup', str)
    return str


@rule ((2, 3, 4), _ ('remove %s') % '\\notes')
def conv (str):
    str = re.sub (r'\\notes\b', '', str)
    return str


@rule ((2, 3, 6), 'lastpagefill -> raggedlastbottom')
def conv (str):
    str = re.sub (r'lastpagefill\s*=\s*"?1"', 'raggedlastbottom = ##t', str)
    return str


@rule ((2, 3, 8), 'remove \\consistsend, strip \\lyrics from \\lyricsto.')
def conv (str):
    str = re.sub (r'\\consistsend', '\\consists', str)
    str = re.sub (r'\\lyricsto\s+("?[a-zA-Z]+"?)(\s*\\new Lyrics\s*)?\\lyrics',
                  r'\\lyricsto \1 \2', str)
    return str


@rule ((2, 3, 9), 'neo_mensural -> neomensural, if-text-padding -> bound-padding')
def conv (str):
    str = re.sub (r'neo_mensural', 'neomensural', str)
    str = re.sub (r'if-text-padding', 'bound-padding', str)
    return str


@rule ((2, 3, 10), '\\addlyrics -> \\oldaddlyrics, \\newlyrics -> \\addlyrics')
def conv (str):
    str = re.sub (r'\\addlyrics', r'\\oldaddlyrics', str)
    str = re.sub (r'\\newlyrics', r'\\addlyrics', str)
    if re.search (r"\\override\s*TextSpanner", str):
        stderr_write ("\nWarning: TextSpanner has been split into DynamicTextSpanner and TextSpanner\n")
    return str


@rule ((2, 3, 11), '\\setMmRestFermata -> ^\\fermataMarkup')
def conv (str):
    str = re.sub (r'\\setMmRestFermata\s+(R[0-9.*/]*)',
                  r'\1^\\fermataMarkup', str)
    return str


@rule ((2, 3, 12), '''\\newpage -> \\pageBreak, junk \\script{up,down,both},
soloADue -> printPartCombineTexts, #notes-to-clusters -> \\makeClusters
''')
def conv (str):
    str = re.sub (r'\\newpage', r'\\pageBreak', str)
    str = re.sub (r'\\scriptUp', r"""{
\\override TextScript  #'direction = #1
\\override Script  #'direction = #1
}""", str)
    str = re.sub (r'\\scriptDown', r"""{
  \\override TextScript  #'direction = #-1
  \\override Script  #'direction = #-1
}""", str)
    str = re.sub (r'\\scriptBoth', r"""{
  \\revert TextScript  #'direction
  \\revert Script  #'direction
}""", str)
    str = re.sub ('soloADue', 'printPartCombineTexts', str)
    str = re.sub (r'\\applymusic\s*#notes-to-clusters',
                      '\\makeClusters', str)

    str = re.sub (r'pagenumber\s*=', 'firstpagenumber = ', str)
    return str


@rule ((2, 3, 16), _ ('''\\foo -> \\foomode (for chords, notes, etc.)
fold \\new FooContext \\foomode into \\foo.'''))
def conv (str):
    str = re.sub (r'\\chords\b', r'\\chordmode', str)
    str = re.sub (r'\\lyrics\b', r'\\lyricmode', str)
    str = re.sub (r'\\figures\b', r'\\figuremode', str)
    str = re.sub (r'\\notes\b', r'\\notemode', str)
    str = re.sub (r'\\drums\b', r'\\drummode', str)
    str = re.sub (r'\\chordmode\s*\\new ChordNames', r'\\chords', str)
    str = re.sub (r'\\new ChordNames\s*\\chordmode', r'\\chords', str)
    str = re.sub (r'\\new FiguredBass\s*\\figuremode', r'\\figures', str)
    str = re.sub (r'\\figuremode\s*\new FiguredBass', r'\\figures', str)
    str = re.sub (r'\\new DrumStaff\s*\\drummode', r'\\drums', str)
    str = re.sub (r'\\drummode\s*\\new DrumStaff', r'\\drums', str)

    return str


@rule ((2, 3, 17), '''slurBoth -> slurNeutral, stemBoth -> stemNeutral, etc.
\\applymusic #(remove-tag 'foo) -> \\removeWithTag 'foo''')
def conv (str):
    str = re.sub (r'(slur|stem|phrasingSlur|tie|dynamic|dots|tuplet|arpeggio|)Both', r'\1Neutral', str)
    str = re.sub (r"\\applymusic\s*#\(remove-tag\s*'([a-z-0-9]+)\)",
                  r"\\removeWithTag #'\1", str)
    return str


@rule ((2, 3, 18), 'Text_item -> Text_interface')
def conv (str):
    str = re.sub (r'Text_item', 'Text_interface', str)
    return str


@rule ((2, 3, 22), 'paper -> layout, bookpaper -> paper')
def conv (str):
    str = re.sub (r'\\paper', r'\\layout', str)
    str = re.sub (r'\\bookpaper', r'\\paper', str)
    if re.search ('paper-set-staff-size', str):
        warning (_ ('''staff size should be changed at top-level
with

  #(set-global-staff-size <STAFF-HEIGHT-IN-POINT>)

'''))


    str = re.sub (r'#\(paper-set-staff-size', '%Use set-global-staff-size at toplevel\n% #(layout-set-staff-size', str)
    return str


@rule ((2, 3, 23), r'\context Foo = NOTENAME -> \context Foo = "NOTENAME"')
def conv (str):
    str = re.sub (r'\\context\s+([a-zA-Z]+)\s*=\s*([a-z]+)\s',
                  r'\\context \1 = "\2" ',
                  str )
    return str


@rule ((2, 3, 24), _ ('''regularize other identifiers'''))
def conv (str):
    def sub(m):
        return regularize_id (m.group (1))
    str = re.sub (r'(maintainer_email|maintainer_web|midi_stuff|gourlay_maxmeasures)',
                  sub, str)
    return str


@rule ((2, 3, 25), 'petrucci_c1 -> petrucci-c1, 1style -> single-digit')
def conv (str):
    str = re.sub ('petrucci_c1', 'petrucci-c1', str)
    str = re.sub ('1style', 'single-digit', str)
    return str


@rule ((2, 4, 0), _ ("bump version for release"))
def conv (str):
    return str


@rule ((2, 5, 0), '\\quote -> \\quoteDuring')
def conv (str):
    str = re.sub (r'\\quote\s+"?([a-zA-Z0-9]+)"?\s+([0-9.*/]+)',
                  r'\\quoteDuring #"\1" { \skip \2 }',
                  str)
    return str


@rule ((2, 5, 1), 'ly:import-module -> ly:module-copy')
def conv (str):
    str = re.sub (r'ly:import-module',
                  r'ly:module-copy', str)
    return str


@rule ((2, 5, 2), '\markup .. < .. > .. -> \markup .. { .. } ..')
def conv (str):
    str = re.sub (r'\\(column|fill-line|dir-column|center-align|right-align|left-align|bracketed-y-column)\s*<(([^>]|<[^>]*>)*)>',
                  r'\\\1 {\2}', str)
    str = re.sub (r'\\(column|fill-line|dir-column|center-align|right-align|left-align|bracketed-y-column)\s*<(([^>]|<[^>]*>)*)>',
                  r'\\\1 {\2}', str)
    str = re.sub (r'\\(column|fill-line|dir-column|center-align|right-align|left-align|bracketed-y-column)\s*<(([^>]|<[^>]*>)*)>',
                  r'\\\1 {\2}', str)
    def get_markup (m):
        s = m.group (0)
        s = re.sub (r'''((\\"|})\s*){''', '\2 \\line {', s)
        return s
    str = re.sub (r'\\markup\s*{([^}]|{[^}]*})*}', get_markup, str)
    return str


@rule ((2, 5, 3), 'ly:find-glyph-by-name -> ly:font-get-glyph, remove - from glyphnames.')
def conv (str):
    str = re.sub ('ly:find-glyph-by-name', 'ly:font-get-glyph', str)
    str = re.sub ('"(scripts|clefs|accidentals)-', r'"\1.', str)
    str = re.sub ("'hufnagel-do-fa", "'hufnagel.do.fa", str)
    str = re.sub ("'(vaticana|hufnagel|medicaea|petrucci|neomensural|mensural)-", r"'\1.", str)
    return str


@rule ((2, 5, 12), '\set Slur #\'dashed = #X -> \slurDashed')
def conv (str):
    str = re.sub (r"\\override\s+(Voice\.)?Slur #'dashed\s*=\s*#\d*(\.\d+)?",
                  r"\\slurDashed", str)
    return str


@rule ((2, 5, 13), _ ('\\encoding: smart recode latin1..utf-8. Remove ly:point-and-click'))
def conv (str):
    input_encoding = 'latin1'
    def func (match):
        encoding = match.group (1)

        # FIXME: automatic recoding of other than latin1?
        if encoding == 'latin1':
            return match.group (2)

        stderr_write (NOT_SMART % ("\\encoding: %s" % encoding))
        stderr_write (_ ("LilyPond source must be UTF-8"))
        stderr_write ('\n')
        if encoding == 'TeX':
            stderr_write (_ ("Try the texstrings backend"))
            stderr_write ('\n')
        else:
            stderr_write ( _("Do something like: %s") % \
                               ("recode %s..utf-8 FILE" % encoding))
            stderr_write ('\n')
        stderr_write (_ ("Or save as UTF-8 in your editor"))
        stderr_write ('\n')
        raise FatalConversionError ()

        return match.group (0)

    str = re.sub (r'\\encoding\s+"?([a-zA-Z0-9]+)"?(\s+)', func, str)

    import codecs
    de_ascii = codecs.getdecoder ('ascii')
    de_utf_8 = codecs.getdecoder ('utf_8')
    de_input = codecs.getdecoder (input_encoding)
    en_utf_8 = codecs.getencoder ('utf_8')
    try:
        de_ascii (str)
    # only in python >= 2.3
    # except UnicodeDecodeError:
    except UnicodeError:
        # do not re-recode UTF-8 input
        try:
            de_utf_8 (str)
        #except UnicodeDecodeError:
        except UnicodeError:
            str = en_utf_8 (de_input (str)[0])[0]



    str = re.sub (r"#\(ly:set-point-and-click '[a-z-]+\)", '', str)
    return str


@rule ((2, 5, 17), _ ('remove %s') % 'ly:stencil-set-extent!')
def conv (str):
    if re.search ("ly:stencil-set-extent!", str):
        stderr_write (NOT_SMART % "ly:stencil-set-extent!")
        stderr_write (_ ('Use %s\n') % '(set! VAR (ly:make-stencil (ly:stencil-expr VAR) X-EXT Y-EXT))')
        raise FatalConversionError ()
    if re.search ("ly:stencil-align-to!", str):
        stderr_write (NOT_SMART % "ly:stencil-align-to!")
        stderr_write (_ ('Use %s\n') % '(set! VAR (ly:stencil-aligned-to VAR AXIS DIR))')
        raise FatalConversionError ()
    return str


@rule ((2, 5, 18), 'ly:warn -> ly:warning')
def conv (str):
    str = re.sub (r"ly:warn\b", 'ly:warning', str)
    return str


@rule ((2, 5, 21), _ ('warn about auto beam settings'))
def conv (str):
    if re.search ("(override-|revert-)auto-beam-setting", str)\
       or re.search ("autoBeamSettings", str):
        stderr_write (NOT_SMART % _ ("auto beam settings"))
        stderr_write (_ ('''
Auto beam settings must now specify each interesting moment in a measure
explicitly; 1/4 is no longer multiplied to cover moments 1/2 and 3/4 too.
'''))
        stderr_write (UPDATE_MANUALLY)
        raise FatalConversionError ()
    return str


@rule ((2, 5, 25), 'unfoldrepeats -> unfoldRepeats, compressmusic -> compressMusic')
def conv (str):
    str = re.sub (r"unfoldrepeats", 'unfoldRepeats', str)
    str = re.sub (r"compressmusic", 'compressMusic', str)
    return str


@rule ((2, 6, 0), _ ("bump version for release"))
def conv (str):
    return str


@rule ((2, 7, 0), 'ly:get-default-font -> ly:grob-default-font')
def conv (str):
    return re.sub('ly:get-default-font', 'ly:grob-default-font', str)


@rule ((2, 7, 1), '''ly:parser-define -> ly:parser-define!
excentricity -> eccentricity
Timing_engraver -> Timing_translator + Default_bar_line_engraver
''')
def conv (str):
    str = re.sub('ly:parser-define', 'ly:parser-define!', str)
    str = re.sub('excentricity', 'eccentricity', str)
    str = re.sub(r'\\(consists|remove) *"?Timing_engraver"?',
                 r'\\\1 "Timing_translator" \\\1 "Default_bar_line_engraver"',
                 str)
    return str


@rule ((2, 7, 2), 'ly:X-moment -> ly:moment-X')
def conv (str):
    str = re.sub('ly:(add|mul|mod|div)-moment', r'ly:moment-\1', str)
    return str


@rule ((2, 7, 4), 'keyAccidentalOrder -> keyAlterationOrder')
def conv (str):
    str = re.sub('keyAccidentalOrder', 'keyAlterationOrder', str)
    return str


@rule ((2, 7, 6), '''Performer_group_performer -> Performer_group, Engraver_group_engraver -> Engraver_group,
inside-slur -> avoid-slur''')
def conv (str):
    str = re.sub('Performer_group_performer', 'Performer_group', str)
    str = re.sub('Engraver_group_engraver', 'Engraver_group', str)
    str = re.sub (r"#'inside-slur\s*=\s*##t *",
                  r"#'avoid-slur = #'inside ", str)
    str = re.sub (r"#'inside-slur\s*=\s*##f *",
                  r"#'avoid-slur = #'around ", str)
    str = re.sub (r"#'inside-slur",
                  r"#'avoid-slur", str)
    return str


@rule ((2, 7, 10), '\\applyxxx -> \\applyXxx')
def conv (str):
    str = re.sub(r'\\applyoutput', r'\\applyOutput', str)
    str = re.sub(r'\\applycontext', r'\\applyContext', str)
    str = re.sub(r'\\applymusic',  r'\\applyMusic', str)
    str = re.sub(r'ly:grob-suicide', 'ly:grob-suicide!', str)
    return str


@rule ((2, 7, 11), '"tabloid" -> "11x17"')
def conv (str):
    str = re.sub(r'\"tabloid\"', '"11x17"', str)
    return str


@rule ((2, 7, 12), 'outputProperty -> overrideProperty')
def conv (str):
    str = re.sub(r'outputProperty' , 'overrideProperty', str)
    return str


@rule ((2, 7, 13), 'layout engine refactoring [FIXME]')
def conv (str):
    def subber (match):
        newkey = {'spacing-procedure': 'springs-and-rods',
                  'after-line-breaking-callback' : 'after-line-breaking',
                  'before-line-breaking-callback' : 'before-line-breaking',
                  'print-function' : 'stencil'} [match.group(3)]
        what = match.group (1)
        grob = match.group (2)

        if what == 'revert':
            return "revert %s #'callbacks %% %s\n" % (grob, newkey)
        elif what == 'override':
            return "override %s #'callbacks #'%s" % (grob, newkey)
        else:
            raise 'urg'
            return ''

    str = re.sub(r"(override|revert)\s*([a-zA-Z.]+)\s*#'(spacing-procedure|after-line-breaking-callback"
                + r"|before-line-breaking-callback|print-function)",
                subber, str)

    if re.search ('bar-size-procedure', str):
        stderr_write (NOT_SMART % "bar-size-procedure")
    if re.search ('space-function', str):
        stderr_write (NOT_SMART % "space-function")
    if re.search ('verticalAlignmentChildCallback', str):
        stderr_write (_ ('verticalAlignmentChildCallback has been deprecated'))
        stderr_write ('\n')
    return str


@rule ((2, 7, 14), _ ('Remove callbacks property, deprecate XY-extent-callback.'))
def conv (str):
    str = re.sub (r"\\override +([A-Z.a-z]+) #'callbacks",
                  r"\\override \1", str)
    str = re.sub (r"\\revert ([A-Z.a-z]+) #'callbacks % ([a-zA-Z]+)",
                  r"\\revert \1 #'\2", str)
    str = re.sub (r"([XY]-extent)-callback", r'\1', str)
    str = re.sub (r"RemoveEmptyVerticalGroup", "VerticalAxisGroup", str)
    str = re.sub (r"\\set ([a-zA-Z]*\.?)minimumVerticalExtent",
                  r"\\override \1VerticalAxisGroup #'minimum-Y-extent",
                  str)
    str = re.sub (r"minimumVerticalExtent",
                  r"\\override VerticalAxisGroup #'minimum-Y-extent",
                  str)
    str = re.sub (r"\\set ([a-zA-Z]*\.?)extraVerticalExtent",
                  r"\\override \1VerticalAxisGroup #'extra-Y-extent", str)
    str = re.sub (r"\\set ([a-zA-Z]*\.?)verticalExtent",
                  r"\\override \1VerticalAxisGroup #'Y-extent", str)
    return str


@rule ((2, 7, 15), _ ('Use grob closures iso. XY-offset-callbacks.'))
def conv (str):
    if re.search ('[XY]-offset-callbacks', str):
        stderr_write (NOT_SMART % "[XY]-offset-callbacks")
    if re.search ('position-callbacks', str):
        stderr_write (NOT_SMART % "position-callbacks")
    return str


@rule ((2, 7, 22), r"\tag #'(a b) -> \tag #'a \tag #'b")
def conv (str):
    def sub_syms (m):
        syms =  m.group (1).split ()
        tags = ["\\tag #'%s" % s for s in syms]
        return ' '.join (tags)

    str = re.sub (r"\\tag #'\(([^)]+)\)",  sub_syms, str)
    return str


@rule ((2, 7, 24), _ ('deprecate %s') % 'number-visibility')
def conv (str):
    str = re.sub (r"#'number-visibility",
                  "#'number-visibility % number-visibility is deprecated. Tune the TupletNumber instead\n",
                  str)
    return str


@rule ((2, 7, 28), "ly:spanner-get-bound -> ly:spanner-bound")
def conv (str):
    str = re.sub (r"ly:spanner-get-bound", "ly:spanner-bound", str)
    return str


@rule ((2, 7, 29), "override Stem #'beamed-* -> #'details #'beamed-*")
def conv (str):
    for a in ['beamed-lengths', 'beamed-minimum-free-lengths',
              'lengths',
              'beamed-extreme-minimum-free-lengths']:
        str = re.sub (r"\\override\s+Stem\s+#'%s" % a,
                      r"\\override Stem #'details #'%s" % a,
                      str)
    return str


@rule ((2, 7, 30), "\\epsfile")
def conv (str):
    str = re.sub (r'\\epsfile *#"', r'\\epsfile #X #10 #"', str)
    return str


@rule ((2, 7, 31), "Foo_bar::bla_bla -> ly:foo-bar::bla-bla")
def conv (str):
    def sub_cxx_id (m):
        str = m.group(1)
        return 'ly:' + str.lower ().replace ('_','-')

    str = re.sub (r'([A-Z][a-z_0-9]+::[a-z_0-9]+)',
                  sub_cxx_id, str)
    return str


@rule ((2, 7, 32), _ ("foobar -> foo-bar for \paper, \layout"))
def conv (str):
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

    for (a,b)  in identifier_subs:
        ### for C++:
        ## str = re.sub ('"%s"' % a, '"%s"' b, str)

        str = re.sub (a, b, str)
    return str


@rule ((2, 7, 32), "debug-beam-quanting -> debug-beam-scoring")
def conv (str):
    str = re.sub ('debug-beam-quanting', 'debug-beam-scoring', str)
    return str


@rule ((2, 7, 36), "def-(music-function|markup-command) -> define-(music-function|markup-command)")
def conv (str):
    str = re.sub ('def-music-function', 'define-music-function', str)
    str = re.sub ('def-markup-command', 'define-markup-command', str)
    return str


@rule ((2, 7, 40), "rehearsalMarkAlignSymbol/barNumberAlignSymbol -> break-align-symbol")
def conv (str):
    str = re.sub (r'\\set\s+Score\s*\.\s*barNumberAlignSymbol\s*=',
                  r"\\override Score.BarNumber #'break-align-symbol = ", str)
    str = re.sub (r'\\set\s*Score\s*\.\s*rehearsalMarkAlignSymbol\s*=',
                  r"\\override Score.RehearsalMark #'break-align-symbol = ", str)
    return str


@rule ((2, 9, 4), "(page-)penalty -> (page-)break-penalty")
def conv (str):
    str = re.sub ('page-penalty', 'page-break-penalty', str)
    str = re.sub ('([^-])penalty', '\1break-penalty', str)
    return str


@rule ((2, 9, 6), "\\context Foo \\applyOutput #bla -> \\applyOutput #'Foo #bla ")
def conv (str):
    str = re.sub (r'\\context\s+\"?([a-zA-Z]+)\"?\s*\\applyOutput', r"\\applyOutput #'\1", str)
    return str


@rule ((2, 9, 9), "annotatefoo -> annotate-foo")
def conv (str):
    str = re.sub ('annotatepage', 'annotate-page', str)
    str = re.sub ('annotateheaders', 'annotate-headers', str)
    str = re.sub ('annotatesystems', 'annotate-systems', str)
    return str


@rule ((2, 9, 11), "\\set tupletNumberFormatFunction -> \\override #'text = ")
def conv (str):
    str = re.sub (r"""(\\set\s)?(?P<context>[a-zA-Z]*.?)tupletNumberFormatFunction\s*=\s*#denominator-tuplet-formatter""",
                  r"""\\override \g<context>TupletNumber #'text = #tuplet-number::calc-denominator-text""", str)

    str = re.sub (r"""(\\set\s+)?(?P<context>[a-zA-Z]*.?)tupletNumberFormatFunction\s*=\s*#fraction-tuplet-formatter""",
                  r"""\\override \g<context>TupletNumber #'text = #tuplet-number::calc-fraction-text""", str)

    if re.search ('tupletNumberFormatFunction', str):
        stderr_write ("\n")
        stderr_write ("tupletNumberFormatFunction has been removed. Use #'text property on TupletNumber")
        stderr_write ("\n")
    return str


@rule ((2, 9, 13), "instrument -> instrumentName, instr -> shortInstrumentName, vocNam -> shortVocalName")
def conv (str):
    str = re.sub ('vocNam', 'shortVocalName', str)
    str = re.sub (r'\.instr\s*=', r'.shortInstrumentName =', str)
    str = re.sub (r'\.instrument\s*=', r'.instrumentName =', str)
    return str


@rule ((2, 9, 16), _ ("deprecate \\tempo in \\midi"))
def conv (str):

    def sub_tempo (m):
        dur = int (m.group (1))
        dots = len (m.group (2))
        count = int (m.group (3))

        log2 = 0
        while dur > 1 :
            dur /= 2
            log2 += 1

        den = (1 << dots) * (1 << log2)
        num = ((1 << (dots+1))  - 1)

        return  """
  \midi {
    \context {
      \Score
      tempoWholesPerMinute = #(ly:make-moment %d %d)
      }
    }

""" % (num*count, den)

    str = re.sub (r'\\midi\s*{\s*\\tempo ([0-9]+)\s*([.]*)\s*=\s*([0-9]+)\s*}', sub_tempo, str)
    return str


@rule ((2, 9, 19), "printfirst-page-number -> print-first-page-number")
def conv (str):
    str = re.sub ('printfirst-page-number', 'print-first-page-number', str)
    return str


@rule ((2, 10, 0), _ ("bump version for release"))
def conv (str):
    return str


@rule ((2, 11, 2), "ly:clone-parser -> ly:parser-clone")
def conv (str):
    return re.sub ('ly:clone-parser',
                   'ly:parser-clone', str)

@rule ((2, 11, 3), "no-spacing-rods -> extra-spacing-width")
def conv (str):
    str = re.sub (r"no-spacing-rods\s+=\s+##t", r"extra-spacing-width = #'(+inf.0 . -inf.0)", str)
    str = re.sub (r"no-spacing-rods\s+=\s+##f", r"extra-spacing-width = #'(0 . 0)", str)
    return str


@rule ((2, 11, 5), _ ("deprecate cautionary-style. Use AccidentalCautionary properties"))
def conv (str):
    str = re.sub ("Accidental\s*#'cautionary-style\s*=\s*#'smaller",
                   "AccidentalCautionary #'font-size = #-2", str)
    str = re.sub ("Accidental\s*#'cautionary-style\s*=\s*#'parentheses",
                   "AccidentalCautionary #'parenthesized = ##t", str)
    str = re.sub ("([A-Za-z]+)\s*#'cautionary-style\s*=\s*#'parentheses",
                   r"\1 #'parenthesized = ##t", str)
    str = re.sub ("([A-Za-z]+)\s*#'cautionary-style\s*=\s*#'smaller",
                   r"\1 #'font-size = #-2", str)
    return str


@rule ((2, 11, 6), _ ("Rename accidental glyphs, use glyph-name-alist."))
def conv (str):

    def sub_acc_name (m):
        idx = int (m.group (1).replace ('M','-'))

        return ["accidentals.doublesharp",
                "accidentals.sharp.slashslash.stemstemstem",
                "accidentals.sharp",
                "accidentals.sharp.slashslash.stem",
                "accidentals.natural",
                "accidentals.mirroredflat",
                "accidentals.flat",
                "accidentals.mirroredflat.flat",
                "accidentals.flatflat"][4-idx]

    str = re.sub (r"accidentals[.](M?[-0-9]+)",
                  sub_acc_name, str)
    str = re.sub (r"(KeySignature|Accidental[A-Za-z]*)\s*#'style\s*=\s*#'([a-z]+)",
                  r"\1 #'glyph-name-alist = #alteration-\2-glyph-name-alist", str)
    ## FIXME: standard vs default, alteration-FOO vs FOO-alteration
    str = str.replace ('alteration-default-glyph-name-alist',
                       'standard-alteration-glyph-name-alist')
    return str


@rule ((2, 11, 10), """allowBeamBreak -> Beam #'breakable = ##t
addquote -> addQuote
""")
def conv (str):
    str = re.sub (r'(\\set\s+)?([A-Z][a-zA-Z]+\s*\.\s*)allowBeamBreak',
                  r"\override \2Beam #'breakable", str)
    str = re.sub (r'(\\set\s+)?allowBeamBreak',
                  r"\override Beam #'breakable", str)
    str = re.sub (r'addquote', 'addQuote', str)
    if re.search ("Span_dynamic_performer", str):
        stderr_write ("Span_dynamic_performer has been merged into Dynamic_performer")

    return str


@rule ((2, 11, 11), "layout-set-staff-size -> layout-set-absolute-staff-size")
def conv (str):
    str = re.sub (r'\(layout-set-staff-size \(\*\s*([0-9.]+)\s*(pt|mm|cm)\)\)',
                  r'(layout-set-absolute-staff-size (* \1 \2))', str)
    return str


@rule ((2, 11, 13), "#'arrow = ##t -> #'bound-details #'right #'arrow = ##t")
def conv (str):
    str = re.sub (r"\\override\s*([a-zA-Z.]+)\s*#'arrow\s*=\s*##t",
                  r"\\override \1 #'bound-details #'right #'arrow = ##t",
                  str)

    if re.search ('edge-text', str):
        stderr_write (NOT_SMART % _ ("edge-text settings for TextSpanner"))
        stderr_write (_ ("Use\n\n%s") %
                          "\t\\override TextSpanner #'bound-details #'right #'text = <right-text>\n"
                          "\t\\override TextSpanner #'bound-details #'left #'text = <left-text>\n")
    return str


@rule ((2, 11, 15), "TextSpanner #'edge-height -> #'bound-details #'right/left #'text = ...\n\
Remove 'forced-distance for fixed spacing between staves in a PianoStaff.")
def conv (str):
    def sub_edge_height (m):
        s = ''
        for (var, h) in [('left', m.group (3)),
                         ('right', m.group (4))]:

            if h and float (h):
                once = m.group (1)
                if not once:
                    once = ''
                context = m.group (2)
                if not context:
                    context = ''

                s += (r"%s \override %sTextSpanner #'bound-details #'%s #'text = \markup { \draw-line #'(0 . %s) }"
                      % (once, context, var, h))

                s += '\n'

        return s


    str = re.sub (r"(\\once)?\s*\\override\s*([a-zA-Z]+\s*[.]\s*)?TextSpanner\s*#'edge-height\s*=\s*#'\(\s*([0-9.-]+)\s+[.]\s+([0-9.-]+)\s*\)", sub_edge_height, str)
    if re.search (r"#'forced-distance", str):
        stderr_write (NOT_SMART % "VerticalAlignment #'forced-distance")
        stderr_write (_ ("Use the `alignment-offsets' sub-property of\n"))
        stderr_write (_ ("NonMusicalPaperColumn #'line-break-system-details\n"))
        stderr_write (_ ("to set fixed distances between staves.\n"))
    return str


@rule ((2, 11, 23), "#'break-align-symbol -> #'break-align-symbols")
def conv (str):
    str = re.sub (r"\\override\s*([a-zA-Z.]+)\s*#'break-align-symbol\s*=\s*#'([a-z-]+)",
                  r"\\override \1 #'break-align-symbols = #'(\2)", str)
    return str


@rule ((2, 11, 35), """scripts.caesura -> scripts.caesura.curved.
""" + _ ("Use #'style not #'dash-fraction to select solid/dashed lines."))
def conv (str):
    str = re.sub (r"scripts\.caesura",
                  r"scripts.caesura.curved", str)

    if re.search ('dash-fraction', str):
        stderr_write (NOT_SMART % _ ("all settings related to dashed lines"))
        stderr_write (_ ("Use \\override ... #'style = #'line for solid lines and\n"))
        stderr_write (_ ("\t\\override ... #'style = #'dashed-line for dashed lines."))
    return str


@rule ((2, 11, 38), """\\setEasyHeads -> \\easyHeadsOn, \\fatText -> \\textLengthOn,
\\emptyText -> \\textLengthOff""")
def conv (str):
    str = re.sub (r"setEasyHeads", r"easyHeadsOn", str)
    str = re.sub (r"fatText", r"textLengthOn", str)
    str = re.sub (r"emptyText", r"textLengthOff", str)
    return str


@rule ((2, 11, 46), "\\set hairpinToBarline -> \\override Hairpin #'to-barline")
def conv (str):
    str = re.sub (r"\\set\s+([a-zA-Z]+)\s*.\s*hairpinToBarline\s*=\s*##([tf]+)",
                  r"\\override \1.Hairpin #'to-barline = ##\2", str)
    str = re.sub (r"\\set\s+hairpinToBarline\s*=\s*##([tf]+)",
                  r"\\override Hairpin #'to-barline = ##\1", str)
    str = re.sub (r"\\unset\s+([a-zA-Z]+)\s*.\s*hairpinToBarline",
                  r"\\revert \1.Hairpin #'to-barline", str)
    str = re.sub (r"\\unset\s+hairpinToBarline",
                  r"\\revert Hairpin #'to-barline", str)
    str = re.sub (r"hairpinToBarline\s*=\s*##([tf]+)",
                  r"\\override Hairpin #'to-barline = ##\1", str)
    str = re.sub (r"\\set (de|)crescendoSpanner = #'dashed-line",
                  r"\\set \1crescendoSpanner = #'text", str)
    return str


@rule ((2, 11, 48), "\\compressMusic -> \\scaleDurations")
def conv (str):
    str = re.sub (r"compressMusic", r"scaleDurations", str)
    return str


@rule ((2, 11, 50), _ ("metronomeMarkFormatter uses text markup as second argument,\n\
fret diagram properties moved to fret-diagram-details."))
def conv (str):
    ## warning 1/2: metronomeMarkFormatter uses text markup as second argument
    if re.search ('metronomeMarkFormatter', str):
        stderr_write (NOT_SMART % "metronomeMarkFormatter")
        stderr_write (_ ("metronomeMarkFormatter got an additional text argument.\n"))
        stderr_write (_ ("The function assigned to Score.metronomeMarkFunction now uses the signature\n%s") %
                          "\t(format-metronome-markup text dur count context)\n")

    ## warning 2/2: fret diagram properties moved to fret-diagram-details
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
      if re.search (prop, str):
          stderr_write (NOT_SMART % (_ ("%s in fret-diagram properties") % prop))
          stderr_write (_ ('Use %s\n') % "fret-diagram-details")
    return str

@rule ((2, 11, 51), "\\octave -> \\octaveCheck, \\arpeggioUp -> \\arpeggioArrowUp,\n\
\\arpeggioDown -> \\arpeggioArrowDown, \\arpeggioNeutral -> \\arpeggioNormal,\n\
\\setTextCresc -> \\crescTextCresc, \\setTextDecresc -> \\dimTextDecresc,\n\
\\setTextDecr -> \\dimTextDecr, \\setTextDim -> \\dimTextDim,\n\
\\setHairpinCresc -> \\crescHairpin, \\setHairpinDecresc -> \\dimHairpin,\n\
\\sustainUp -> \\sustainOff, \\sustainDown -> \\sustainOn\n\
\\sostenutoDown -> \\sostenutoOn, \\sostenutoUp -> \\sostenutoOff")
def conv (str):
    str = re.sub (r"\\octave(?![a-zA-Z])", r"\\octaveCheck", str)
    str = re.sub (r"arpeggioUp", r"arpeggioArrowUp", str)
    str = re.sub (r"arpeggioDown", r"arpeggioArrowDown", str)
    str = re.sub (r"arpeggioNeutral", r"arpeggioNormal", str)
    str = re.sub (r"setTextCresc", r"crescTextCresc", str)
    str = re.sub (r"setTextDecresc", r"dimTextDecresc", str)
    str = re.sub (r"setTextDecr", r"dimTextDecr", str)
    str = re.sub (r"setTextDim", r"dimTextDim", str)
    str = re.sub (r"setHairpinCresc", r"crescHairpin", str)
    str = re.sub (r"setHairpinDecresc", r"dimHairpin", str)
    str = re.sub (r"sustainUp", r"sustainOff", str)
    str = re.sub (r"sustainDown", r"sustainOn", str)
    str = re.sub (r"sostenutoDown", r"sostenutoOn", str)
    str = re.sub (r"sostenutoUp", r"sostenutoOff", str)
    return str

@rule ((2, 11, 52), "\\setHairpinDim -> \\dimHairpin")
def conv (str):
    str = str.replace ("setHairpinDim", "dimHairpin")
    return str

@rule ((2, 11, 53), "infinite-spacing-height -> extra-spacing-height")
def conv (str):
    str = re.sub (r"infinite-spacing-height\s+=\s+##t", r"extra-spacing-height = #'(-inf.0 . +inf.0)", str)
    str = re.sub (r"infinite-spacing-height\s+=\s+##f", r"extra-spacing-height = #'(0 . 0)", str)
    return str

@rule ((2, 11, 55), "#(set-octavation oct) -> \\ottava #oct,\n\
\\put-adjacent markup axis dir markup -> \\put-adjacent axis dir markup markup")
def conv (str):
    str = re.sub (r"#\(set-octavation (-*[0-9]+)\)", r"\\ottava #\1", str)
    if re.search ('put-adjacent', str):
        stderr_write (NOT_SMART % _ ("\\put-adjacent argument order"))
        stderr_write (_ ("Axis and direction now come before markups:\n"))
        stderr_write (_ ("\\put-adjacent axis dir markup markup."))
        stderr_write ("\n")
    return str

@rule ((2, 11, 57), "\\center-align -> \\center-column, \\hcenter -> \\center-align")
def conv (str):
    str = re.sub (r"([\\:]+)center-align", r"\1center-column", str)
    str = re.sub (r"hcenter(\s+)", r"center-align\1", str)
    return str

@rule ((2, 11, 60), "printallheaders -> print-all-headers")
def conv (str):
    str = re.sub (r"printallheaders", r"print-all-headers", str)
    return str

@rule ((2, 11, 61), "gregorian-init.ly -> gregorian.ly")
def conv (str):
    str = re.sub (r'\\include(\s+)"gregorian-init.ly"', r'\\include\1"gregorian.ly"', str)
    return str

@rule ((2, 11, 62), "makam-init.ly -> makam.ly, \\bigger -> \\larger")
def conv (str):
    str = re.sub (r'\\include(\s+)"makam-init.ly"', r'\\include\1"makam.ly"', str)
    str = re.sub (r"([\\:])bigger", r"\1larger", str)
    return str

@rule ((2, 11, 64), "systemSeparatorMarkup -> system-separator-markup,\n\
InnerStaffGroup -> StaffGroup, InnerChoirStaff -> ChoirStaff")
def conv (str):
    str = re.sub (r'systemSeparatorMarkup', r'system-separator-markup', str)
    if re.search (r'\\InnerStaffGroup', str):
        stderr_write (NOT_SMART % _("re-definition of InnerStaffGroup"))
        stderr_write (FROM_TO % ("InnerStaffGroup", "StaffGroup"))
        stderr_write (UPDATE_MANUALLY)
        raise FatalConversionError ()
    if re.search (r'\\InnerChoirStaff', str):
        stderr_write (NOT_SMART % _("re-definition of InnerChoirStaff"))
        stderr_write (FROM_TO % ("InnerChoirStaff", "ChoirStaff"))
        stderr_write (UPDATE_MANUALLY)
        raise FatalConversionError ()
    else:
        str = re.sub ('InnerStaffGroup', 'StaffGroup', str)
        str = re.sub ('InnerChoirStaff', 'ChoirStaff', str)
    return str

@rule ((2, 12, 0),
       _ ("Syntax changes for \\addChordShape and \\chord-shape") + "\n" + \
       _ ("bump version for release"))
def conv(str):
    if re.search(r'\\addChordShape', str):
        stderr_write (NOT_SMART % "addChordShape")
        stderr_write (_ ("stringTuning must be added to addChordShape call.\n"))
        stderr_write (UPDATE_MANUALLY)
        raise FatalConversionError ()
    if re.search (r'\\chord-shape', str):
        stderr_write (NOT_SMART % "chord-shape")
        stderr_write (_ ("stringTuning must be added to chord-shape call.\n"))
        stderr_write (UPDATE_MANUALLY)
        raise FatalConversionError ()
    return str

@rule ((2,12,3),
    _ ("Remove oldaddlyrics"))
def conv(str):
    if re.search(r'\\oldaddlyrics', str):
        stderr_write (NOT_SMART % "oldaddlyrics")
        stderr_write (_ ("oldaddlyrics is no longer supported. \n \
        Use addlyrics or lyrsicsto instead.\n"))
        stderr_write (UPDATE_MANUALLY)
        raise FatalConversionError ()
    return str

@rule ((2, 13, 0), _ ("keySignature property not reversed any more\n\
MIDI 47: orchestral strings -> orchestral harp"))
def conv(str):
    if re.search(r'\set Staff.keySignature', str):
        stderr_write (NOT_SMART % "Staff.keySignature")
        stderr_write (_ ("The alist for Staff.keySignature is no \
longer in reversed order.\n"))
    str = str.replace('"orchestral strings"', '"orchestral harp"')
    return str

@rule ((2, 13, 1),
       _ ("\\bar \".\" now produces a thick barline\n\
ly:hairpin::after-line-breaking -> ly:spanner::kill-zero-spanned-time\n\
Dash parameters for slurs and ties are now in dash-definition"))
def conv(str):
    if re.search(r'\\bar\s*"\."', str):
        stderr_write (NOT_SMART % "\\bar \".\"")
        stderr_write (_ ("\\bar \".\" now produces a thick barline.\n"))
        stderr_write (UPDATE_MANUALLY)
    str = re.sub (r'ly:hairpin::after-line-breaking', r'ly:spanner::kill-zero-spanned-time', str)
    if re.search("(Slur|Tie)\w+#\'dash-fraction", str) \
        or re.search("(Slur|Tie)\w+#\'dash-period", str):
        stderr_write (NOT_SMART % "dash-fraction, dash-period")
        stderr_write (_ ("Dash parameters for slurs and ties are now in \'dash-definition.\n"))
        stderr_write (UPDATE_MANUALLY)
    return str

@rule ((2, 13, 4),
       _ ("Autobeaming rules have changed.  override-auto-beam-setting and\n\
revert-auto-beam-setting have been eliminated.\n\
\\overrideBeamSettings has been added.\n\
beatGrouping has been eliminated.\n\
Different settings for vertical layout.\n\
ly:system-start-text::print -> system-start-text::print\n\
Beam #'thickness -> Beam #'beam-thickness\n\
ly:note-head::brew-ez-stencil -> note-head::brew-ez-stencil\n\
ly:ambitus::print -> ambitus::print\n\
Explicit dynamics context definition from `Piano centered dynamics'\n\
template replaced by new `Dynamics' context."))
def conv(str):
    if re.search("override-auto-beam-setting", str):
        stderr_write (NOT_SMART % "override-auto-beam-setting")
        stderr_write (_ (" \
   Autobeam settings are now overriden with \\overrideBeamSettings.\n"))
        stderr_write (UPDATE_MANUALLY)
    if re.search("revert-auto-beam-setting", str):
        stderr_write (NOT_SMART % "override-auto-beam-setting")
        stderr_write (_ (" \
   Autobeam settings are now reverted with \\revertBeamSettings.\n"))
        stderr_write (UPDATE_MANUALLY)
    str = re.sub(r"\\set\s+beatGrouping", r"\\setBeatGrouping", str)
    if re.search(r"\w+\s*.\s*beatGrouping", str):
        stderr_write (NOT_SMART % "beatGrouping")
        stderr_write (_ (" \
   beatGrouping with a specified context must now be accomplished with\n\
   \\overrideBeamSettings.\n"))
        stderr_write (UPDATE_MANUALLY)
    if re.search(r'alignment-offsets', str):
        stderr_write (NOT_SMART % "alignment-offsets")
        stderr_write (_ ("alignment-offsets has been changed to alignment-distances: \
you must now specify the distances between staves rather than the offset of staves.\n"))
        stderr_write (UPDATE_MANUALLY)
    str = re.sub ('ly:(system-start-text::print|note-head::brew-ez-stencil|ambitus::print)',
                  '\\1', str)
    str = re.sub ('(\\bBeam\\s+#\')(?=thickness\\b)', '\\1beam-', str)
    str = re.sub (r'(\\context\s*\{{1}[^\}]+\\type\s+\"?Engraver_group\"?\s+\\name\s+"*Dynamics"*[^\}]*\}{1})',
                  '% [Convert-ly] The Dynamics context is now included by default.', str)
    return str

@rule ((2, 13, 10),
       _ ("Remove obsolete engravers/translators: Note_swallow_translator,\n\
Rest_swallow_translator, Skip_event_swallow_translator, Swallow_engraver,\n\
Swallow_performer and String_number_engraver.\n\
New vertical spacing variables."))
def conv(str):
    str = re.sub (r'\\(consists|remove)\s+"*(Swallow_(engraver|performer)|'
                  '(Note|Rest|Skip_event)_swallow_translator|String_number_engraver)"*',
                  '', str)

    # match through the end of assignments in the form "x = 30", "x = 1 \in", or "x = #3"
    str = re.sub (r"(page-top-space)\s*=\s*(([+-]?[.\d]*\s*\\[-\w]+)|(#?\s*[-+]?[.\d]+))",
                  r"obsolete-\g<0>"
                  r"  top-system-spacing #'space = #(/ obsolete-\1 staff-space)",
                  str)
    str = re.sub (r"(between-system-space)\s*=\s*(([+-]?[.\d]*\s*\\[-\w]+)|(#?\s*[-+]?[.\d]+))",
                  r"obsolete-\g<0>"
                  r"  between-system-spacing #'space = #(/ obsolete-\1 staff-space)"
                  r"  between-scores-system-spacing #'space = #(/ obsolete-\1 staff-space)",
                  str)
    str = re.sub (r"(between-system-padding)\s*=\s*(([+-]?[.\d]*\s*\\[-\w]+)|(#?\s*[-+]?[.\d]+))",
                  r"obsolete-\g<0>"
                  r"  between-system-spacing #'padding = #(/ obsolete-\1 staff-space)"
                  r"  between-scores-system-spacing #'padding = #(/ obsolete-\1 staff-space)",
                  str)
    str = re.sub (r"((before|between|after)-title-space)\s*=\s*(([+-]?[.\d]*\s*\\[-\w]+)|(#?\s*[-+]?[.\d]+))",
                  r"obsolete-\g<0>"
                  r"  \2-title-spacing #'space = #(/ obsolete-\1 staff-space)",
                  str)

    if re.search(r"VerticalAxisGroup\s*#\s*'minimum-Y-extent", str):
        stderr_write (NOT_SMART % "minimum-Y-extent")
        stderr_write (_ ("Vertical spacing no longer depends on the Y-extent of a VerticalAxisGroup.\n"))
        stderr_write (UPDATE_MANUALLY)

    return str

@rule ((2, 13, 16),
       _ ("Unify fetaNumber and fetaDynamic encodings"))
def conv(str):
    return re.sub(r'\bfeta(Number|Dynamic)', 'fetaText', str)

@rule ((2, 13, 18),
       _ ("\\RemoveEmpty*StaffContext -> \\*Staff \\RemoveEmptyStaves"))
def conv(str):
    str = re.sub (r"\\RemoveEmpty(|Drum|Rhythmic|Tab)StaffContext",
                  r"\\\1Staff \\RemoveEmptyStaves",
                  str);
    str = re.sub (r"\\AncientRemoveEmptyStaffContext",
                  r"\\VaticanaStaff \\RemoveEmptyStaves",
                  str);
    return str

@rule ((2, 13, 20),
       _ ("\\cresc etc. are now postfix operators"))
def conv (str):
    str = re.sub (r'\\(cresc|dim|endcresc|enddim)\b', r'\\deprecated\1', str)
    return str

@rule ((2, 13, 27),
       ("interval-translate -> coord-translate"))
def conv (str):
    str = re.sub ('interval-translate', 'coord-translate', str)
    return str

@rule ((2, 13, 29),
       _ ("Eliminate beamSettings, beatLength, \\setBeatGrouping, \\overrideBeamSettings and \\revertBeamSettings.\n\
\"accordion.accEtcbase\" -> \"accordion.etcbass\""))
def conv(str):
    def sub_acc (m):
        d = {
            'Dot': 'dot',
            'Discant': 'discant',
            'Bayanbase': 'bayanbass',
            'Stdbase': 'stdbass',
            'Freebase': 'freebass',
            'OldEE': 'oldEE'
            }
        return '"accordion.%s"' %  d[m.group (1)]

    str = re.sub (r'"accordion\.acc([a-zA-Z]+)"',
                  sub_acc, str)
    if re.search(r'overrideBeamSettings', str):
        stderr_write (NOT_SMART % "\\overrideBeamSettings")
        stderr_write (_ ("Use \\set beamExceptions or \\overrideTimeSignatureSettings.\n"))
        stderr_write (UPDATE_MANUALLY)
    if re.search(r'revertBeamSettings', str):
        stderr_write (NOT_SMART % "\\revertBeamSettings")
        stderr_write (_ ("Use \\set beamExceptions or \\revertTimeSignatureSettings.\n"))
        stderr_write (UPDATE_MANUALLY)
    if re.search(r'beamSettings', str):
        stderr_write (NOT_SMART % "beamSettings")
        stderr_write (_ ("Use baseMoment, beatStructure, and beamExceptions.\n"))
        stderr_write (UPDATE_MANUALLY)
    if re.search(r'beatLength', str):
        stderr_write (NOT_SMART % "beatLength")
        stderr_write (_ ("Use baseMoment and beatStructure.\n"))
        stderr_write (UPDATE_MANUALLY)
    if re.search(r'setBeatGrouping', str):
        stderr_write (NOT_SMART % "setbeatGrouping")
        stderr_write (_ ("Use baseMoment and beatStructure.\n"))
        stderr_write (UPDATE_MANUALLY)
    return str

@rule ((2, 13, 31),
    _ ("Woodwind diagrams: Move size, thickness, and graphic from argument list to properties.\n\
Deprecate negative dash-period for hidden lines: use #'style = #'none instead."))
def conv(str):
    if re.search(r'woodwind-diagram', str):
        stderr_write (NOT_SMART % "woodwind-diagrams")
        stderr_write (_ ("Move size, thickness, and graphic to properties.  Argument should be just the key list.\n"))
        stderr_write (UPDATE_MANUALLY)
    str = re.sub (r"dash-period\s+=\s*#\s*-[0-9.]+",
                  r"style = #'none",
                  str);
    return str

@rule ((2, 13, 36),
    _ ("Rename vertical spacing variables.\n\
Add fretboard-table argument to savePredefinedFretboard."))
def conv(str):
    str = re.sub ('after-title-spacing',           'markup-system-spacing', str)
    str = re.sub ('before-title-spacing',          'score-markup-spacing',  str)
    str = re.sub ('between-scores-system-spacing', 'score-system-spacing',  str)
    # this rule also converts page-breaking-between-system-spacing:
    str = re.sub ('between-system-spacing',        'system-system-spacing', str)
    str = re.sub ('between-title-spacing',         'markup-markup-spacing', str)
    str = re.sub ('bottom-system-spacing',         'last-bottom-spacing',   str)
    str = re.sub ('top-title-spacing',             'top-markup-spacing',    str)

    str = re.sub (r"storePredefinedDiagram",
                  r"storePredefinedDiagram #default-fret-table",
                  str);
    return str

@rule ((2, 13, 39),
    _ ("Rename vertical spacing grob properties."))
def conv(str):
    # this rule also converts default-next-staff-spacing:
    str = re.sub ('next-staff-spacing',       'staff-staff-spacing',             str)
    # this is not a mistake:
    #   Both 'next- and 'between- become 'staff-staff-spacing.
    #   There is no conflict since they are in different grobs.
    str = re.sub ('between-staff-spacing',    'staff-staff-spacing',             str)
    str = re.sub ('after-last-staff-spacing', 'staffgroup-staff-spacing',        str)
    str = re.sub ('inter-staff-spacing',      'nonstaff-relatedstaff-spacing',   str)
    str = re.sub ('non-affinity-spacing',     'nonstaff-unrelatedstaff-spacing', str)
    str = re.sub ('inter-loose-line-spacing', 'nonstaff-nonstaff-spacing',       str);

    return str

@rule ((2, 13, 40),
    _ ("Remove \\paper variables head-separation and foot-separation."))
def conv(str):
    if re.search (r'head-separation', str):
        stderr_write (NOT_SMART % "head-separation")
        stderr_write (_ ("Adjust settings for top-system-spacing instead.\n"))
        stderr_write (UPDATE_MANUALLY)
    if re.search (r'foot-separation', str):
        stderr_write (NOT_SMART % "foot-separation")
        stderr_write (_ ("Adjust settings for last-bottom-spacing instead.\n"))
        stderr_write (UPDATE_MANUALLY);

    return str

@rule ((2, 13, 42),
    _ ("Rename space to basic-distance in various spacing alists.\n\
Remove HarmonicParenthesesItem grob."))
def conv(str):
    str = re.sub (r'\(space\s+\.\s+([0-9]*\.?[0-9]*)\)', r'(basic-distance . \1)', str)
    str = re.sub (r"#'space\s+=\s+#?([0-9]*\.?[0-9]*)", r"#'basic-distance = #\1", str)
    if re.search (r'HarmonicParenthesesItem', str):
        stderr_write (NOT_SMART % "HarmonicParenthesesItem")
        stderr_write (_ ("HarmonicParenthesesItem has been eliminated.\n"))
        stderr_write (_ ("Harmonic parentheses are part of the TabNoteHead grob.\n"))
        stderr_write (UPDATE_MANUALLY);
    return str

@rule ((2, 13, 44),
    _ ("Remove context from overrideTimeSignatureSettings and revertTimeSignatureSettings.\n"))

def conv(str):
    str = re.sub (r"\\(override|revert)TimeSignatureSettings(\s+[^#]*)(#[^#]*)#", r"\\\1TimeSignatureSettings\2#", str)
    return str

@rule ((2, 13, 46),
    _ ("Change stringTunings from a list of semitones to a list of pitches.\n"\
       "Change tenor and baritone ukulele names in string tunings.\n"\
       "Generate messages for manual conversion of vertical spacing if required."))

def conv(str):
    def semitones2pitch(semitones):
        steps = [0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6]
        alterations = ["NATURAL", "SHARP", "NATURAL", "SHARP", "NATURAL", "NATURAL", "SHARP", "NATURAL", "SHARP", "NATURAL", "SHARP", "NATURAL"]
        octave = 0
        while semitones > 11:
            octave += 1
            semitones -=12
        while semitones < 0:
            octave -= 1
            semitones += 12
        pitchArgs = "%d %d %s" % (octave, steps[semitones], alterations[semitones])
        return pitchArgs

    def convert_tones (semitone_list):
        tones = semitone_list.split ()
        res = ""
        for tone in tones:
            args = semitones2pitch(int(tone))
            res += ",(ly:make-pitch " + args + ") "
        return res

    def new_tunings (matchobj):
        return "stringTunings = #`(" + convert_tones(matchobj.group(1)) + ")"
    str = re.sub (r"stringTunings\s*=\s*#'\(([\d\s-]*)\)", \
          new_tunings , str)

    str = re.sub (r"ukulele-(tenor|baritone)-tuning", r"\1-ukulele-tuning", str)

    if re.search (r"[^-]page-top-space", str):
        stderr_write (NOT_SMART % "page-top-space")
        stderr_write (UPDATE_MANUALLY)
    if re.search (r"[^-]between-system-(space|padding)", str):
        stderr_write (NOT_SMART % "between-system-space, -padding")
        stderr_write (UPDATE_MANUALLY)
    if re.search (r"[^-](before|between|after)-title-space", str):
        stderr_write (NOT_SMART % "before-, between-, after-title-space")
        stderr_write (UPDATE_MANUALLY)
    if re.search (r"\\name\s", str):
        stderr_write ("\n" + _("Vertical spacing changes might affect user-defined contexts.") + "\n")
        stderr_write (UPDATE_MANUALLY)

    return str

@rule ((2, 13, 48),
       _ ("Replace bar-size with bar-extent."))

def conv(str):
    def size_as_extent (matchobj):
        half = "%g" % (float (matchobj.group (1)) / 2)
        return "bar-extent = #'(-" + half + " . " + half + ")"

    str = re.sub (r"bar-size\s*=\s*#([0-9\.]+)", size_as_extent, str)

    return str

@rule ((2, 13, 51),
    _ ("Woodwind diagrams: Changes to the clarinet diagram."))
def conv(str):
    if re.search (r'\\woodwind-diagram\s*#[^#]*clarinet\s', str):
        stderr_write (NOT_SMART % "woodwind-diagrams")
        stderr_write (_ ("Clarinet fingering changed to reflect actual anatomy of instrument.\n"))
        stderr_write (UPDATE_MANUALLY)
    return str

@rule ((2, 14, 0),
       _ ("bump version for release"))
def conv (str):
    return str

@rule ((2, 15, 7),
    _ ("Handling of non-automatic footnotes."))
def conv(str):
    if re.search (r'\\footnote', str):
        stderr_write (NOT_SMART % "\\footnote")
        stderr_write (_ ("If you are using non-automatic footnotes, make sure to set footnote-auto-numbering = ##f in the paper block.\n"))
        stderr_write (UPDATE_MANUALLY)
    return str

@rule ((2, 15, 9),
       _ ("Change in internal property for MultiMeasureRest"))
def conv (str):
    if re.search (r'use-breve-rest',str):
        stderr_write (NOT_SMART % "use-breve-rest")
        stderr_write (_ ("This internal property has been replaced by round-up-to-longer-rest, round-up-exceptions and usable-duration-logs.\n"))
        stderr_write (UPDATE_MANUALLY)
    return str

@rule ((2, 15, 10),
       _ ("Creation of a Flag grob and moving of certain Stem properties to this grob"))
def conv (str):
    str = re.sub (r"Stem\s+#'flag-style", r"Flag #'style", str)
    str = re.sub (r"Stem\s+#'stroke-style", r"Flag #'stroke-style", str)
    str = re.sub (r"Stem\s+#'flag", r"Flag #'stencil", str)
    str = re.sub (r"(\s+(?:\\once\s*)?)\\override\s+Stem\s+#'transparent\s*=\s*##t", r"\g<1>\\override Stem #'transparent = ##t\g<1>\\override Flag #'transparent = ##t", str)
    str = re.sub (r"(\s+(?:\\once\s*)?)\\revert\s*Stem\s+#'transparent", r"\g<1>\\revert Stem #'transparent\g<1>\\revert Flag #'transparent", str)
    str = re.sub (r"(\s+(?:\\once\s*)?)\\override\s+Stem\s+#'stencil\s*=\s*##f", r"\g<1>\\override Stem #'stencil = ##f\g<1>\\override Flag #'stencil = ##f", str)
    str = re.sub (r"(\s+(?:\\once\s*)?)\\revert\s*Stem\s+#'stencil", r"\g<1>\\revert Stem #'stencil\g<1>\\revert Flag #'stencil", str)
    return str

@rule ((2, 15, 16), r"\makeStringTuning, \contextStringTuning -> \stringTuning")
def conv (str):
    str = re.sub (r"(\s+)\\contextStringTuning(\s+)#'([-a-zA-Z]+)(\s+<[^<>]+>)",
                  r"""\g<1>#(define \g<3> #{ \\stringTuning\g<4> #})\g<1>\\set stringTunings = #\g<3>""",
                  str)
    str = re.sub (r"""
\\makeStringTuning(\s+)#'([-a-zA-Z]+)""",
                  r"""
"\g<2>" = \\stringTuning""", str)
    str = re.sub (r"\\makeStringTuning(\s+)#'([-a-zA-Z]+)(\s+<[^<>]+>)",
                  r"#(define \g<2> #{ \\stringTuning\g<3> #})", str)
    return str

@rule ((2, 15, 17), "\\markuplines -> \\markuplist\n\
Change Beam broken slope syntax.")
def conv (str):
    str = re.sub (r"""
\\markuplines( +)([^ ].*)
            \1([^ ])""", r"""
\\markuplist\g<1>\g<2>
           \g<1>\g<3>""", str)
    str = re.sub (r"\\markuplines", r"\\markuplist", str)
    str = re.sub (r"@funindex markuplines", r"@funindex markuplist", str)
    if re.search (r'consistent-broken-slope', str):
        stderr_write (NOT_SMART % "consistent-broken-slope")
        stderr_write (_ ("consistent-broken-slope is now handled through the positions callback.\n"))
        stderr_write (_ ("input/regression/beam-broken-classic.ly shows how broken beams are now handled.\n"))
        stderr_write (UPDATE_MANUALLY)
    return str

def paren_matcher (n):
    # poor man's matched paren scanning, gives up
    # after n+1 levels.  Matches any string with balanced
    # parens inside; add the outer parens yourself if needed.
    # Nongreedy.
    return r"[^()]*?(?:\("*n+r"[^()]*?"+r"\)[^()]*?)*?"*n
    return

def undollar_scm (m):
    return re.sub (r"\$(.?)", r"\1", m.group (0))

def undollar_embedded (m):
    str = re.sub (r"#\$", "#", m.group (1))
    # poor man's matched paren scanning after #, gives up
    # after 25 levels.
    str = re.sub ("#`?\("+paren_matcher (25)+"\)", undollar_scm, str)
    return m.string[m.start (0):m.start (1)] + str + m.string[m.end (1):m.end (0)]

def strip_export (str):
    return re.sub (r"\(ly:export\s+(" + paren_matcher (25) + r")\)",
                   r"\1", str)

def export_puller (m):
    if not re.search (r"ly:export\s+", m.group (0)):
        return m.group (0)
    return "$" + strip_export (m.string[m.start (0)+1:m.end (0)])

def ugly_function_rewriter (m):
    return m.string[m.start(0):m.start(1)] + strip_export (m.group (1)) + m.string[m.end(1):m.end(0)]

should_really_be_music_function = "(?:\
set-time-signature|empty-music|add-grace-property|\
remove-grace-property|set-accidental-style)"

def record_ugly (m):
    global should_really_be_music_function
    if not re.match (should_really_be_music_function, m.group (1)) \
            and re.search (r"ly:export\s+", m.group (2)):
        should_really_be_music_function = \
            should_really_be_music_function[:-1] + "|" + m.group (1) + ")"
    return m.group (0)

@rule ((2, 15, 18), "#$ -> #, ly:export -> $")
def conv (str):
    str = re.sub (r"(?s)#@?\{(.*?)#@?\}", undollar_embedded, str)
    str = re.sub (r"#\(define(?:-public)?\s+\(([-a-zA-Z]+)"
                  + r"\b[^()]*?\)(" + paren_matcher (25)
                  + r")\)", record_ugly, str)
    str = re.sub (r"\(define(?:-public)?\s+\(" + should_really_be_music_function
                  + r"\b[^()]*\)(" + paren_matcher (25)
                  + r")\)", ugly_function_rewriter, str)
    str = re.sub (r"#(?=\(" + should_really_be_music_function + ")", "$", str)
    str = re.sub (r"#\(markup\*(?=\s)", r"$(markup", str)
    str = re.sub ("#\("+paren_matcher (25)+"\)", export_puller, str)
    if re.search (r"\(ly:export\s+", str):
        stderr_write (NOT_SMART % "ly:export")
    return str

@rule ((2, 15, 19), r"$(set-time-signature ...) -> \time")
def conv (str):
    str = re.sub (r"\$\(set-time-signature\s+([0-9]+)\s+([0-9]+)\s*\)",
                  r"\\time \1/\2", str)
    str = re.sub (r"\$\(set-time-signature\s+([0-9]+)\s+([0-9]+)\s+(" +
                  paren_matcher (5) + r")\)", r"\\time #\3 \1/\2", str)
    if re.search (r"\(set-time-signature\s+", str):
        stderr_write (NOT_SMART % "set-time-signature")
    return str

@rule ((2, 15, 20), r"$(set-accidental-style ...) -> \accidentalStyle")
def conv (str):
    str = re.sub (r"\$\(set-accidental-style\s+'([-a-z]+)\)",
                  r'\\accidentalStyle "\1"', str)
    str = re.sub (r"\$\(set-accidental-style\s+'([-a-z]+)\s+'([-A-Za-z]+)\s*\)",
                  r'''\\accidentalStyle #'\2 "\1"''', str)
    str = re.sub (r"(@funindex\s+)set-accidental-style",
                  r"\1\\accidentalStyle", str)
    return str

def brace_matcher (n):
    # poor man's matched brace scanning, gives up
    # after n+1 levels.  Matches any string with balanced
    # braces inside; add the outer braces yourself if needed.
    # Nongreedy.
    return r"[^{}]*?(?:{"*n+r"[^{}]*?"+r"}[^{}]*?)*?"*n

matchstring = r'"(?:[^"\\]|\\.)*"'
matcharg = (r"\s+(?:[$#]['`]?\s*(?:[a-zA-Z]\S*|" + matchstring + r"|\("
            + paren_matcher(20) + r"\))|" + matchstring + r"|\\[a-z_A-Z]+)")
matchmarkup = (r'(?:\\markup\s*(?:{' + brace_matcher (20) +r'}|' +
               matchstring + r'|(?:\\[a-z_A-Z][a-z_A-Z-]*(?:' + matcharg +
               r')*?\s*)*(?:' + matchstring + "|{" + brace_matcher (20) +
               "}))|" + matchstring + ")")

@rule((2, 15, 25), r"\(auto)?Footnote(Grob)? -> \footnote")
def conv (str):
    # The following replacement includes the final markup argument in
    # the match in order to better avoid touching the equally named
    # markup function.  The other functions have unique names, so
    # there is no point in including their last, possibly complex
    # argument in the match.
    str = re.sub (r"\\footnote(" + matcharg + (r")(\s*" + matchmarkup)*2 + ")",
                  r"\\footnote\2\1\3", str)
    str = re.sub (r"\\footnoteGrob"+("(" + matcharg + ")")*2 + r"(\s*" + matchmarkup + ")",
                  r"\\footnote\3\2\1", str)
    str = re.sub (r"\\autoFootnoteGrob" + ("(" + matcharg + ")")*2,
                  r"\\footnote\2\1", str)
    str = re.sub (r"\\autoFootnote",
                  r"\\footnote", str)
    return str

@rule((2, 15, 32), r"tempoWholesPerMinute -> \tempo")
def conv (str):
    def sub_tempo (m):
        num = int (m.group (1))
        den = int (m.group (2))

        if (den & (den - 1)) != 0 :
            return m.group (0)

        # Don't try dotted forms if they result in less than 30 bpm.
        # It is not actually relevant to get this right since this
        # only occurs in non-printing situations
        if den >= 16 and (num % 7) == 0 and num >= 210 :
            return r"\tempo %d.. = %d" % (den/4, num/7)

        if den >= 8 and (num % 3) == 0 and num >= 90 :
            return r"\tempo %d. = %d" % (den/2, num/3)

        return r"\tempo %d = %d" % (den, num)

    str = re.sub (r"\\context\s*@?\{\s*\\Score\s+tempoWholesPerMinute\s*=\s*" +
                  r"#\(ly:make-moment\s+([0-9]+)\s+([0-9]+)\)\s*@?\}",
                  sub_tempo, str)
    return str

@rule((2, 15, 39), r"\footnote ... -> \footnote ... \default")
def conv (str):
    def not_first (s):
        def match_fun (m):
            if m.group (1):
                return m.group (0)
            return m.expand (s)
        return match_fun
    str = re.sub ("(" + matchmarkup + ")|"
                  + r"(\\footnote(?:\s*"
                  + matchmarkup + ")?" + matcharg + "(?:" + matcharg
                  + ")?\s+" + matchmarkup + ")",
                  not_first (r"\2 \\default"), str)
    return str

@rule ((2, 15, 40), r"Remove beamWholeMeasure")
def conv (str):
    if re.search (r"\bbeamWholeMeasure\b", str):
        stderr_write (NOT_SMART % "beamWholeMeasure")
        stderr_write (_ ("beamExceptions controls whole-measure beaming.") + "\n")
    return str

@rule ((2, 15, 42), r"\set stringTuning -> \set Staff.stringTuning")
def conv (str):
    str = re.sub (r"(\\set\s+)stringTuning", r"\1Staff.stringTuning", str)
    return str

wordsyntax = r"[a-zA-Z\200-\377](?:[-_]?[a-zA-Z\200-\377])*"

@rule ((2, 15, 43), r'"custom-tuning" = -> custom-tuning =')
def conv (str):
    str = re.sub ('\n"(' + wordsyntax + r')"(\s*=\s*\\stringTuning)', "\n\\1\\2", str)
    return str

@rule ((2, 16, 0),
       _ ("bump version for release"))
def conv (str):
    return str

@rule ((2, 17, 0), r"blank-*-force -> blank-*-penalty")
def conv (str):
    str = re.sub ('blank-page-force', 'blank-page-penalty', str)
    str = re.sub ('blank-last-page-force', 'blank-last-page-penalty', str)
    str = re.sub ('blank-after-score-page-force', 'blank-after-score-page-penalty', str)
    return str


@rule ((2, 17, 4), r"\shape Grob #offsets -> \shape #offsets Grob")
def conv (str):
    str = re.sub (r"\\shape(\s+(?:[a-zA-Z]+|" + matchstring + "))(" +
                  matcharg + ")", r"\\shape\2\1", str)
    return str

barstring=r"(\\bar|whichBar|defaultBarType|segnoType|doubleRepeatType|startRepeatType|endRepeatType|doubleRepeatSegnoType|startRepeatSegnoType|endRepeatSegnoType)(\s*[=]?\s*[#]?)"

@rule ((2, 17, 5), r"New bar line interface")
def conv(str):
    str = re.sub (barstring + r'"\|:"', '\\1\\2".|:"', str)
    str = re.sub (barstring + r'":\|"', '\\1\\2":|."', str)
    str = re.sub (barstring + r'"\|\|:"', '\\1\\2".|:-||"', str)
    str = re.sub (barstring + r'":\|:"', '\\1\\2":..:"', str)
    str = re.sub (barstring + r'"\.\|\."', '\\1\\2".."', str)
    str = re.sub (barstring + r'"\|S"', '\\1\\2"S-|"', str)
    str = re.sub (barstring + r'"S\|"', '\\1\\2"S-S"', str)
    str = re.sub (barstring + r'":\|S"', '\\1\\2":|.S"', str)
    str = re.sub (barstring + r'":\|S\."', '\\1\\2":|.S-S"', str)
    str = re.sub (barstring + r'"S\|:"', '\\1\\2"S.|:-S"', str)
    str = re.sub (barstring + r'"\.S\|:"', '\\1\\2"S.|:"', str)
    str = re.sub (barstring + r'":\|S\|:"', '\\1\\2":|.S.|:"', str)
    str = re.sub (barstring + r'":\|S\.\|:"', '\\1\\2":|.S.|:-S"', str)
    str = re.sub (barstring + r'":"', '\\1\\2";"', str)
    str = re.sub (barstring + r'"\|s"', '\\1\\2"|-s"', str)
    str = re.sub (barstring + r'"dashed"', '\\1\\2"!"', str)
    str = re.sub (barstring + r'"kievan"', '\\1\\2"k"', str)
    str = re.sub (barstring + r'"empty"', '\\1\\2"-"', str)
    return str

symbol_list = (r"#'(?:" + wordsyntax + r"|\(\s*(?:" + wordsyntax + r"\s+)*"
               + wordsyntax + r"\s*\))")

grob_path = r"(?:" + symbol_list + r"\s+)*" + symbol_list

grob_spec = wordsyntax + r"(?:\s*\.\s*" + wordsyntax + r")?"

def path_replace (m):
    return m.group (1) + string.join (re.findall (wordsyntax, m.group (2)), ".")

@rule ((2, 17, 6), r"""\accidentalStyle #'Context "style" -> \accidentalStyle Context.style
\alterBroken "Context.grob" -> \alterBroken Context.grob
\overrideProperty "Context.grob" -> \overrideProperty Context.grob
\tweak Grob #'symbol -> \tweak Grob.symbol""")
def conv (str):
    def patrep (m):
        def fn_path_replace (m):
            x = string.join (re.findall (wordsyntax, m.group (2)), ".")
            if x in ["TimeSignature", "KeySignature", "BarLine",
                     "Clef", "StaffSymbol", "OttavaBracket",
                     "LedgerLineSpanner"]:
                x = "Staff." + x
            return m.group (1) + x
        if m.group (1):
            return m.group (0)
        x = m.group (2) + m.group (4)

        if m.group (3):
            x = x + re.sub (r"(\s*)(" + symbol_list + ")", fn_path_replace,
                            m.group (3))

            if not m.group (5):
                x = r"\single" + x
        return x

    str = re.sub (r'''(\\accidentalStyle\s+)#?"([-A-Za-z]+)"''',
                  r"\1\2", str)
    str = re.sub (r'''(\\accidentalStyle\s+)#'([A-Za-z]+)\s+#?"?([-A-Za-z]+)"?''',
                  r"\1\2.\3", str)
    str = re.sub (r'''(\\(?:alterBroken|overrideProperty)\s+)#?"([A-Za-z]+)\s*\.\s*([A-Za-z]+)"''',
                  r"\1\2.\3", str)
    str = re.sub (r'''(\\tweak\s+)#?"?([A-Za-z]+)"?\s+?#'([-A-Za-z]+)''',
                  r"\1\2.\3", str)
    str = re.sub (r'''(\\tweak\s+)#'([-A-Za-z]+)''',
                  r"\1\2", str)
    str = re.sub ("(" + matchmarkup + ")|"
                  + r"(\\footnote(?:\s*"
                  + matchmarkup + ")?" + matcharg + ")(" + matcharg
                  + r")?(\s+" + matchmarkup + r")(\s+\\default)?",
                  patrep, str)
    str = re.sub (r'''(\\alterBroken)(\s+[A-Za-z.]+)(''' + matcharg
                  + matcharg + ")", r"\1\3\2", str)
    str = re.sub (r"(\\overrideProperty\s+)(" + grob_spec + r"\s+" + grob_path + ")",
                  path_replace, str)
    str = re.sub (r"(\\(?:override|revert)\s+)(" + grob_spec + r"\s+" + grob_path + ")",
                  path_replace, str)
    return str

@rule ((2, 17, 11), r"""\times -> \tuplet, \set tupletSpannerDuration -> \tupletSpan
(ly:make-moment 1 4) -> (ly:make-moment 1/4)
(ly:make-duration 0 0 1 2) -> (ly:make-duration 0 0 1/2)""")
def conv(str):
    def sub_dur (m):
        num = int (m.group (1))
        den = int (m.group (2))

# if den is no power of 2, don't even try to use an unscaled duration
        if (den & (den - 1)) != 0 :
            return (r"\tupletSpan 1*%d/%d" % (num, den))

        if den >= 4 and num == 7 :
            return (r"\tupletSpan %d.." % (den/4))

        if den >= 2 and num == 3 :
            return (r"\tupletSpan %d." % (den/2))

        if num == 1 :
            return (r"\tupletSpan %d" % den)

        return (r"\tupletSpan 1*%d/%d" % (num, den))

    str = re.sub (r"\\set\s+tupletSpannerDuration\s*=\s*" +
                  r"#\(ly:make-moment\s+([0-9]+)\s+([0-9]+)\s*\)",
                  sub_dur, str)
    str = re.sub (r"\\unset tupletSpannerDuration",
                  r"\\tupletSpan \\default", str)
    str = re.sub (r"\\times(\s*)([0-9]+)/([0-9]+)",
                  r"\\tuplet\1\3/\2", str)

    str = re.sub (r"(\(ly:make-moment\s+-?[0-9]+)\s+([1-9][0-9]*\))",
                  r"\1/\2", str)
    str = re.sub (r"(\(ly:make-moment\s+-?[0-9]+)\s+([0-9]+\s+-?[0-9]+)\s([0-9]+\))",
                  r"\1/\2/\3", str)
    str = re.sub (r"(\(ly:make-duration\s+-?[0-9]+\s+[0-9]+\s+[0-9]+)\s+([0-9]+\))",
                  r"\1/\2", str)
    return str

@rule((2, 17, 14), r"\accepts ... -> \accepts ... \defaultchild ...")
def conv(str):
    def matchaccepts(m):
        # First weed out definitions starting from an existing
        # definition: we assume that the inherited \defaultchild is
        # good enough for our purposes.  Heuristic: starts with a
        # backslash and an uppercase letter.
        if re.match (r"\s*\\[A-Z]", m.group (1)):
            return m.group (0)
        # existing defaultchild obviously trumps all
        if re.search (r"\\defaultchild[^-_a-zA-Z]", m.group (1)):
            return m.group (0)
        # take the first \\accepts if any and replicate it
        return re.sub ("(\r?\n[ \t]*|[ \t]+)"
                       + r"""\\accepts(\s+(?:#?".*?"|[-_a-zA-Z]+))""",
                       r"\g<0>\1\\defaultchild\2",
                       m.group (0), 1)

    str = re.sub (r"\\context\s*@?\{(" + brace_matcher (20) + ")\}",
                  matchaccepts, str)
    return str

@rule((2, 17, 15), r"""#(ly:set-option 'old-relative)
\relative -> \relative c'""")
def conv(str):
    if re.search (r"[#$]\(ly:set-option\s+'old-relative", str):
        stderr_write (NOT_SMART % "#(ly:set-option 'old-relative)")
        stderr_write (UPDATE_MANUALLY)
        raise FatalConversionError ();
    # If the file contains a language switch to a language where the
    # name of c is not "c", we can't reliably know which parts of the
    # file will need "c" and which need "do".
    m = re.search (r'\\language\s(?!\s*#?"(?:nederlands|deutsch|english|norsk|suomi|svenska))"', str)
    if m:
        # Heuristic: if there is a non-commented { before the language
        # selection, we can't be sure.
        # Also if there is any selection of a non-do language.
        if (re.search ("^[^%\n]*\\{", m.string[:m.start()], re.M)
            or re.search ('\\language\s(?!\s*#?"(?:catalan|espanol|español|italiano|français|portugues|vlaams))"', str)):
            do = "$(ly:make-pitch 0 0)"
        else:
            do = "do'"
    else:
        do = "c'"
    str = re.sub (r"(\\relative)(\s+(\{|[\\<]))",
                  r"\1 " + do + r"\2", str)
    return str

@rule ((2, 17, 18),
    "Rename OctavateEight to ClefModifier, rename related properties.")
def conv(str):
    str = re.sub ('OctavateEight',               'ClefModifier',                   str)
    str = re.sub ('octavate-eight-interface',    'clef-modifier-interface',        str)
    str = re.sub ('clefOctavation',              'clefTransposition',              str)
    str = re.sub ('clefOctavationFormatter',     'clefTranspositionFormatter',     str)
    str = re.sub ('clefOctavationStyle',         'clefTranspositionStyle',         str)
    str = re.sub ('cueClefOctavation',           'cueClefTransposition',           str)
    str = re.sub ('cueClefOctavationFormatter',  'cueClefTranspositionFormatter',  str)
    str = re.sub ('cueClefOctavationStyle',      'cueClefTranspositionStyle',      str)
    return str

@rule((2, 17, 19), r"\column { \vspace #2 } -> \column { \combine \null \vspace #2 }")
def conv(str):
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

        str = re.sub (r"(\\\\?)vspace(\s)", r"\1combine \1null \1vspace\2", m.group(0))
        return str

    str = re.sub (r"\\(?:left-|right-|center-|)column\s*\{" + brace_matcher (20) + r"\}",
                  vspace_replace, str)
    return str

@rule((2, 17, 20), _(r"Flag.transparent and Flag.color inherit from Stem"))
def conv(str):
    str = re.sub (r"(((?:\\once\s*)?)\\override\s+((?:\w+\.)?)Stem\.(transparent|color)\s*=\s*(#\S+))\s+\2\\override\s+\3Flag\.\4\s*=\s*\5",
                  r"\1", str)
    str = re.sub (r"(((?:\\once\s*)?)\\revert\s+((?:\w+\.)?)Stem\.(transparent|color))\s+\2\\revert\s+\3Flag\.\4",
                  r"\1", str)
    str = re.sub (r"(\\tweak\s+((?:\w+\.)?)Stem\.(transparent|color)\s+(#\S+))\s+\\tweak\s+\2Flag\.\3\s+\4",
                  r"\1", str)
    return str

@rule((2, 17, 25), r'''\tempo 4. = 50~60 -> \tempo 4. = 50-60
-| -> -!
pipeSymbol, escapedParenthesisOpenSymbol ... -> "|", "\\(" ...''')
def conv(str):
#  This goes for \tempo commands ending with a range, like
#  = 50 ~ 60
#  and uses - instead.  We don't explicitly look for \tempo since the
#  complete syntax has a large number of variants, and this is quite
#  unlikely to occur in other contexts
    str = re.sub (r"(=\s*[0-9]+\s*)~(\s*[0-9]+\s)", r"\1-\2", str)
# Match strings, and articulation shorthands that end in -^_
# so that we leave alone -| in quoted strings and c4--|
    def subnonstring(m):
        if m.group (1):
            return m.group (1)+"!"
        return m.group (0)
    str = re.sub (r"([-^_])\||" + matchstring + r"|[-^_][-^_]", subnonstring, str)
    str = re.sub (r"\bdashBar\b", "dashBang", str)
    orig = [ "pipeSymbol",
             "bracketOpenSymbol",
             "bracketCloseSymbol",
             "tildeSymbol",
             "parenthesisOpenSymbol",
             "parenthesisCloseSymbol",
             "escapedExclamationSymbol",
             "escapedParenthesisOpenSymbol",
             "escapedParenthesisCloseSymbol",
             "escapedBiggerSymbol",
             "escapedSmallerSymbol" ]
    repl = [ r'"|"',
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
    words = r"\b(?:(" + ")|(".join (orig) + r"))\b"
    def wordreplace(m):
        def instring(m):
            return re.sub (r'["\\]',r'\\\g<0>',repl[m.lastindex-1])
        if m.lastindex:
            return repl[m.lastindex-1]
        return '"' + re.sub (words, instring, m.group(0)[1:-1]) + '"'
    str = re.sub (words + "|" + matchstring, wordreplace, str)
    return str

@rule((2, 17, 27), r'''\stringTuning \notemode -> \stringTuning''')
def conv(str):
    str = re.sub (r"\\stringTuning\s*\\notemode(\s*)@?\{\s*(.*?)\s*@?}",
                  r"\\stringTuning\1\2", str)
    if re.search (r'[^-\w]staff-padding[^-\w]', str):
        stderr_write (NOT_SMART % "staff-padding")
        stderr_write (_ ("Staff-padding now controls the distance to the baseline, not the nearest point."))
    return str

@rule((2, 17, 29), r'''Dynamic_engraver -> New_dynamic_engraver+Dynamic_align_engraver
New_dynamic_engraver -> Dynamic_engraver''')
def conv(str):
    str = re.sub ("(\r?\n?[ \t]*\\\\(?:consists|remove)\\s*)(\"?)Dynamic_engraver\\2",
                  r"\1\2New_dynamic_engraver\2\1\2Dynamic_align_engraver\2",
                  str)
# Should we warn about any remaining Dynamic_engraver?  Possibly it
# will do the job just fine.
    str = re.sub ("New_dynamic_engraver", "Dynamic_engraver", str)
    return str

@rule ((2, 17, 97), r'''(make-relative (a b) b ...) -> make-relative (a b) #{ a b #}...''')
def conv (str):
    str = re.sub (r"(\(make-relative\s+\(\s*(([A-Za-z][-_A-Za-z0-9]*)" +
                  r"(?:\s+[A-Za-z][-_A-Za-z0-9]*)*)\s*\)\s*)\3(?=\s)",
                  r"\1(make-event-chord (list \2))", str)
    str = re.sub (r"(\(make-relative\s+\(\s*([A-Za-z][-_A-Za-z0-9]*" +
                  r"(?:\s+([A-Za-z][-_A-Za-z0-9]*))+)\s*\)\s*)\3(?=\s)",
                  r"\1(make-sequential-music (list \2))", str)
    return str

@rule ((2, 18, 0),
       _ ("bump version for release"))
def conv (str):
    return str

@rule ((2, 19, 2), r"\lyricsto \new/\context/... -> \new/\context/... \lyricsto")
def conv (str):
    word=r'(?:#?"[^"]*"|\b' + wordsyntax + r'\b)'
    str = re.sub (r"(\\lyricsto\s*" + word + r"\s*)(\\(?:new|context)\s*" + word
                  + r"(?:\s*=\s*" + word + r")?\s*)",
                  r"\2\1", str)
    str = re.sub (r"(\\lyricsto\s*" + word + r"\s*)\\lyricmode\b\s*",
                  r"\1", str)
    str = re.sub (r"(\\lyricsto\s*" + word + r"\s*)\\lyrics\b\s*",
                  r"\\new Lyrics \1", str)
    str = re.sub (r'\\lyricmode\s*(\\lyricsto\b)', r"\1", str)
    return str

@rule ((2, 19, 7), "keySignature -> keyAlterations")
def conv(str):
    str = re.sub (r'\bkeySignature\b', 'keyAlterations', str)
    str = re.sub (r'\blastKeySignature\b', 'lastKeyAlterations', str)
    str = re.sub (r'\blocalKeySignature\b', 'localAlterations', str)
    return str

@rule ((2, 19, 11), "thin-kern -> segno-kern")
def conv(str):
    str = re.sub (r'\bthin-kern\b', 'segno-kern', str)
    return str

# before_id is written in a manner where it will only substantially
# (rather than as a lookbefore assertion) match material that could
# not be part of a previous id.  In that manner, one replacement does
# not inhibit an immediately adjacent replacement.

before_id = r'(?:^|(?<!\\)(?:\\\\)+|(?<=[^-_\\a-zA-Z])|(?<=[^a-zA-Z][-_]))'

# after_id is a pure lookbehind assertion so its match string is
# always empty

after_id = r'(?![a-zA-Z]|[-_][a-zA-Z])'

@rule ((2, 19, 16), """implicitTimeSignatureVisibility -> initialTimeSignatureVisibility
csharp -> c-sharp""")
def conv(str):
    str = re.sub (r'\bimplicitTimeSignatureVisibility\b', 'initialTimeSignatureVisibility', str)
    str = re.sub ('(' + before_id + r'[a-g])((?:sharp){1,2}|(?:flat){1,2})'
                  + after_id, r'\1-\2', str)
    return str

# Guidelines to write rules (please keep this at the end of this file)
#
# - keep at most one rule per version; if several conversions should be done,
# concatenate them into a single "conv" function;
#
# - enclose strings to be localized with `_(' and  `)';
#
# - write rule for bumping major stable version with
#
#     _ ("bump version for release")
#
# as exact description.
