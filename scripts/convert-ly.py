#!@PYTHON@
#
# convert-ly.py -- Update old LilyPond input files (fix name?)
#
# source file of the GNU LilyPond music typesetter
#
# (c) 1998--2004  Han-Wen Nienhuys <hanwen@cs.uu.nl>
#                 Jan Nieuwenhuizen <janneke@gnu.org>


# TODO
#   use -f and -t for -s output

# NEWS
# 0.2
#  - rewrite in python

import os
import sys
import __main__
import getopt
import  string
import re
import time

program_name = sys.argv[0]

version = '@TOPLEVEL_VERSION@'

# Did we ever have \mudela-version?  I doubt it.
# lilypond_version_re_str = '\\\\version *\"(.*)\"'
lilypond_version_re_str = '\\\\(mudela-)?version *\"([^"]+)\"'
lilypond_version_re = re.compile (lilypond_version_re_str)
add_version = 1


def program_id ():
	return '%s (GNU LilyPond) %s' % (program_name, version)

def identify ():
	sys.stderr.write (program_id () + '\n')

def usage ():
	sys.stdout.write (
		r"""Usage: %s [OPTIONS]... [FILE]...
Try to convert to newer lilypond-versions.  The version number of the
input is guessed by default from \version directive.

Options:
  -h, --help             print this help
  -e, --edit             edit in place
  -f, --from=VERSION     start from version [default: \version found in file]
  -s, --show-rules       print all rules
  -t, --to=VERSION       end at version [default: @TOPLEVEL_VERSION@]
  -n, --no-version       don't add new version stamp
      --version          print program version

Report bugs to bugs-gnu-music@gnu.org.

""" % program_name)
	
	
	sys.exit (0)

def print_version ():
	
	sys.stdout.write (r"""%s

This is free software.  It is covered by the GNU General Public
License, and you are welcome to change it and/or distribute copies of
it under certain conditions.  invoke as `%s --warranty' for more
information.

""" % (program_id (), program_name))
	
def str_to_tuple (s):
	return tuple (map (string.atoi, string.split (s, '.')))

def tup_to_str (t):
	return string.join (map (lambda x: '%s' % x, list (t)), '.')

def version_cmp (t1, t2):
	for x in [0, 1, 2]:
		if t1[x] - t2[x]:
			return t1[x] - t2[x]
	return 0

def guess_lilypond_version (filename):
	s = open (filename).read ()
	m = lilypond_version_re.search (s)
	if m:
		return m.group (2)
	else:
		return ''

class FatalConversionError:
	pass

conversions = []

def show_rules (file):
	for x in conversions:
		if (not from_version or x[0] > from_version) \
		   and (not to_version or x[0] <= to_version):
			file.write  ('%s: %s\n' % (tup_to_str (x[0]), x[2]))

############################
		
if 1:
	def conv(str):
		if re.search ('\\\\multi', str):
			sys.stderr.write ('\nNot smart enough to convert \\multi')
		return str
	
	conversions.append (((0,1,9), conv, '\\header { key = concat + with + operator }'))

if 1:					# need new a namespace
	def conv (str):
		if re.search ('\\\\octave', str):
			sys.stderr.write ('\nNot smart enough to convert \\octave')
		#	raise FatalConversionError()
		
		return str

	conversions.append ((
		((0,1,19), conv, 'deprecated \\octave; can\'t convert automatically')))


if 1:					# need new a namespace
	def conv (str):
		str = re.sub ('\\\\textstyle([^;]+);',
					 '\\\\property Lyrics . textstyle = \\1', str)
		# harmful to current .lys
		# str = re.sub ('\\\\key([^;]+);', '\\\\accidentals \\1;', str)
			
		return str

	conversions.append ((
		((0,1,20), conv, 'deprecated \\textstyle, new \\key syntax')))


if 1:
	def conv (str):
		str = re.sub ('\\\\musical_pitch', '\\\\musicalpitch',str)
		str = re.sub ('\\\\meter', '\\\\time',str)
			
		return str

	conversions.append ((
		((0,1,21), conv, '\\musical_pitch -> \\musicalpitch, '+
		 '\\meter -> \\time')))

if 1:
	def conv (str):
		return str

	conversions.append ((
		((1,0,0), conv, '0.1.21 -> 1.0.0 ')))


if 1:
	def conv (str):
		str = re.sub ('\\\\accidentals', '\\\\keysignature',str)
		str = re.sub ('specialaccidentals *= *1', 'keyoctaviation = 0',str)
		str = re.sub ('specialaccidentals *= *0', 'keyoctaviation = 1',str)
			
		return str

	conversions.append ((
		((1,0,1), conv, '\\accidentals -> \\keysignature, ' +
		 'specialaccidentals -> keyoctaviation')))

if 1:
	def conv(str):
		if re.search ('\\\\header', str):
			sys.stderr.write ('\nNot smart enough to convert to new \\header format')
		return str
	
	conversions.append (((1,0,2), conv, '\\header { key = concat + with + operator }'))

if 1:
	def conv(str):
		str =  re.sub ('\\\\melodic([^a-zA-Z])', '\\\\notes\\1',str)
		return str
	
	conversions.append (((1,0,3), conv, '\\melodic -> \\notes'))

if 1:
	def conv(str):
		str =  re.sub ('default_paper *=', '',str)
		str =  re.sub ('default_midi *=', '',str)
		return str
	
	conversions.append (((1,0,4), conv, 'default_{paper,midi}'))

if 1:
	def conv(str):
		str =  re.sub ('ChoireStaff', 'ChoirStaff',str)
		str =  re.sub ('\\\\output', 'output = ',str)
			
		return str
	
	conversions.append (((1,0,5), conv, 'ChoireStaff -> ChoirStaff'))

if 1:
	def conv(str):
		if re.search ('[a-zA-Z]+ = *\\translator',str):
			sys.stderr.write ('\nNot smart enough to change \\translator syntax')
		#	raise FatalConversionError()
		return str
	
	conversions.append (((1,0,6), conv, 'foo = \\translator {\\type .. } ->\\translator {\\type ..; foo; }'))


if 1:
	def conv(str):
		str =  re.sub ('\\\\lyrics*', '\\\\lyrics',str)
			
		return str
	
	conversions.append (((1,0,7), conv, '\\lyric -> \\lyrics'))

if 1:
	def conv(str):
		str =  re.sub ('\\\\\\[/3+', '\\\\times 2/3 { ',str)
		str =  re.sub ('\\[/3+', '\\\\times 2/3 { [',str)
		str =  re.sub ('\\\\\\[([0-9/]+)', '\\\\times \\1 {',str)
		str =  re.sub ('\\[([0-9/]+)', '\\\\times \\1 { [',str)
		str =  re.sub ('\\\\\\]([0-9/]+)', '}', str)
		str =  re.sub ('\\\\\\]', '}',str)
		str =  re.sub ('\\]([0-9/]+)', '] }', str)
		return str
	
	conversions.append (((1,0,10), conv, '[2/3 ]1/1 -> \\times 2/3 '))

if 1:
	def conv(str):
		return str
	conversions.append (((1,0,12), conv, 'Chord syntax stuff'))


if 1:
	def conv(str):
		
		
		str =  re.sub ('<([^>~]+)~([^>]*)>','<\\1 \\2> ~', str)
			
		return str
	
	conversions.append (((1,0,13), conv, '<a ~ b> c -> <a b> ~ c'))

if 1:
	def conv(str):
		str =  re.sub ('<\\[','[<', str)
		str =  re.sub ('\\]>','>]', str)
			
		return str
	
	conversions.append (((1,0,14), conv, '<[a b> <a b]>c -> [<a b> <a b>]'))


if 1:
	def conv(str):
		str =  re.sub ('\\\\type([^\n]*engraver)','\\\\TYPE\\1', str)
		str =  re.sub ('\\\\type([^\n]*performer)','\\\\TYPE\\1', str)
		str =  re.sub ('\\\\type','\\\\context', str)
		str =  re.sub ('\\\\TYPE','\\\\type', str)
		str =  re.sub ('textstyle','textStyle', str)
			
		return str
	
	conversions.append (((1,0,16), conv, '\\type -> \\context, textstyle -> textStyle'))


if 1:
	def conv(str):
		if re.search ('\\\\repeat',str):
			sys.stderr.write ('\nNot smart enough to convert \\repeat')
		#	raise FatalConversionError()
		return str
	
	conversions.append (((1,0,18), conv,
                '\\repeat NUM Music Alternative -> \\repeat FOLDSTR Music Alternative'))

if 1:
	def conv(str):
		str =  re.sub ('SkipBars','skipBars', str)
		str =  re.sub ('fontsize','fontSize', str)
		str =  re.sub ('midi_instrument','midiInstrument', str)			
			
		return str

	conversions.append (((1,0,19), conv,
                'fontsize -> fontSize, midi_instrument -> midiInstrument, SkipBars -> skipBars'))


if 1:
	def conv(str):
		str =  re.sub ('tieydirection','tieVerticalDirection', str)
		str =  re.sub ('slurydirection','slurVerticalDirection', str)
		str =  re.sub ('ydirection','verticalDirection', str)			
			
		return str

	conversions.append (((1,0,20), conv,
                '{,tie,slur}ydirection -> {v,tieV,slurV}erticalDirection'))


if 1:
	def conv(str):
		str =  re.sub ('hshift','horizontalNoteShift', str)
			
		return str

	conversions.append (((1,0,21), conv,
                'hshift -> horizontalNoteShift'))


if 1:
	def conv(str):
		str =  re.sub ('\\\\grouping[^;]*;','', str)
			
		return str

	conversions.append (((1,1,52), conv,
                'deprecate \\grouping'))


if 1:
	def conv(str):
		str =  re.sub ('\\\\wheel','\\\\coda', str)
			
		return str

	conversions.append (((1,1,55), conv,
                '\\wheel -> \\coda'))

if 1:
	def conv(str):
		str =  re.sub ('keyoctaviation','keyOctaviation', str)
		str =  re.sub ('slurdash','slurDash', str)
			
		return str

	conversions.append (((1,1,65), conv,
                'slurdash -> slurDash, keyoctaviation -> keyOctaviation'))

if 1:
	def conv(str):
		str =  re.sub ('\\\\repeat *\"?semi\"?','\\\\repeat "volta"', str)
			
		return str

	conversions.append (((1,1,66), conv,
                'semi -> volta'))


if 1:
	def conv(str):
		str =  re.sub ('\"?beamAuto\"? *= *\"?0?\"?','noAutoBeaming = "1"', str)
			
		return str

	conversions.append (((1,1,67), conv,
                'beamAuto -> noAutoBeaming'))

if 1:
	def conv(str):
		str =  re.sub ('automaticMelismas', 'automaticMelismata', str)
			
		return str

	conversions.append (((1,2,0), conv,
                'automaticMelismas -> automaticMelismata'))

if 1:
	def conv(str):
		str =  re.sub ('dynamicDir\\b', 'dynamicDirection', str)
			
		return str

	conversions.append (((1,2,1), conv,
                'dynamicDir -> dynamicDirection'))

if 1:
	def conv(str):
		str =  re.sub ('\\\\cadenza *0 *;', '\\\\cadenzaOff', str)
		str =  re.sub ('\\\\cadenza *1 *;', '\\\\cadenzaOn', str)		
			
		return str

	conversions.append (((1,3,4), conv,
                '\\cadenza -> \\cadenza{On|Off}'))

if 1:
	def conv (str):
		str = re.sub ('"?beamAuto([^"=]+)"? *= *"([0-9]+)/([0-9]+)" *;*',
			      'beamAuto\\1 = #(make-moment \\2 \\3)',
			      str)
		return str

	conversions.append (((1,3,5), conv, 'beamAuto moment properties'))

if 1:
	def conv (str):
		str = re.sub ('stemStyle',
			      'flagStyle',
			      str)
		return str

	conversions.append (((1,3,17), conv, 'stemStyle -> flagStyle'))

if 1:
	def conv (str):
		str = re.sub ('staffLineLeading',
			      'staffSpace',
			      str)
		return str

	conversions.append (((1,3,18), conv, 'staffLineLeading -> staffSpace'))


if 1:
	def conv(str):
		if re.search ('\\\\repetitions',str):
			sys.stderr.write ('\nNot smart enough to convert \\repetitions')
		#	raise FatalConversionError()
		return str
	
	conversions.append (((1,3,23), conv,
                '\\\\repetitions feature dropped'))


if 1:
	def conv (str):
		str = re.sub ('textEmptyDimension *= *##t',
			      'textNonEmpty = ##f',
			      str)
		str = re.sub ('textEmptyDimension *= *##f',
			      'textNonEmpty = ##t',
			      str)
		return str

	conversions.append (((1,3,35), conv, 'textEmptyDimension -> textNonEmpty'))

if 1:
	def conv (str):
		str = re.sub ("([a-z]+)[ \t]*=[ \t]*\\\\musicalpitch *{([- 0-9]+)} *\n",
			      "(\\1 . (\\2))\n", str)
		str = re.sub ("\\\\musicalpitch *{([0-9 -]+)}",
			      "\\\\musicalpitch #'(\\1)", str)
		if re.search ('\\\\notenames',str):
			sys.stderr.write ('\nNot smart enough to convert to new \\notenames format')
		return str

	conversions.append (((1,3,38), conv, '\musicalpitch { a b c } -> #\'(a b c)'))

if 1:
	def conv (str):
		def replace (match):
			return '\\key %s;' % string.lower (match.group (1))
		
		str = re.sub ("\\\\key ([^;]+);",  replace, str)
		return str
	
	conversions.append (((1,3,39), conv, '\\key A ;  ->\\key a;'))

if 1:
	def conv (str):
		if re.search ('\\[:',str):
			sys.stderr.write ('\nNot smart enough to convert to new tremolo format')
		return str

	conversions.append (((1,3,41), conv,
                '[:16 c4 d4 ] -> \\repeat "tremolo" 2 { c16 d16 }'))

if 1:
	def conv (str):
		str = re.sub ('Staff_margin_engraver' , 'Instrument_name_engraver', str)
		return str

	conversions.append (((1,3,42), conv,
                'Staff_margin_engraver deprecated, use Instrument_name_engraver'))

if 1:
	def conv (str):
		str = re.sub ('note[hH]eadStyle\\s*=\\s*"?(\\w+)"?' , "noteHeadStyle = #'\\1", str)
		return str

	conversions.append (((1,3,49), conv,
                'noteHeadStyle value: string -> symbol'))

if 1:
	def conv (str):
		if re.search ('\\\\keysignature', str):
			sys.stderr.write ('\nNot smart enough to convert to new tremolo format')
		return str


	conversions.append (((1,3,58), conv,
                'noteHeadStyle value: string -> symbol'))

if 1:
	def conv (str):
		str = re.sub (r"""\\key *([a-z]+) *;""", r"""\\key \1 \major;""",str);
		return str
	conversions.append (((1,3,59), conv,
                '\key X ; -> \key X major; '))

if 1:
	def conv (str):
		str = re.sub (r'latexheaders *= *"\\\\input ',
			      'latexheaders = "',
			      str)
		return str
	conversions.append (((1,3,68), conv, 'latexheaders = "\\input global" -> latexheaders = "global"'))




# TODO: lots of other syntax change should be done here as well
if 1:
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
	
	conversions.append (((1,3,92), conv, 'basicXXXProperties -> XXX, Repeat_engraver -> Volta_engraver'))

if 1:
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
		# ugh, we loose context setting here...
		str = re.sub ('\\\\property *[^ ]*verticalDirection[^=]*= *#?"?(1|(\\\\up))"?', '\\\\stemUp\\\\slurUp\\\\tieUp', str)
		str = re.sub ('\\\\property *[^ ]*verticalDirection[^=]*= *#?"?((-1)|(\\\\down))"?', '\\\\stemDown\\\\slurDown\\\\tieDown', str)
		str = re.sub ('\\\\property *[^ ]*verticalDirection[^=]*= *#?"?(0|(\\\\center))"?', '\\\\stemBoth\\\\slurBoth\\\\tieBoth', str)

		str = re.sub ('verticalDirection[^=]*= *#?"?(1|(\\\\up))"?', 'Stem \\\\override #\'direction = #0\nSlur \\\\override #\'direction = #0\n Tie \\\\override #\'direction = #1', str)
		str = re.sub ('verticalDirection[^=]*= *#?"?((-1)|(\\\\down))"?', 'Stem \\\\override #\'direction = #0\nSlur \\\\override #\'direction = #0\n Tie \\\\override #\'direction = #-1', str)
		str = re.sub ('verticalDirection[^=]*= *#?"?(0|(\\\\center))"?', 'Stem \\\\override #\'direction = #0\nSlur \\\\override #\'direction = #0\n Tie \\\\override #\'direction = #0', str)
		
		str = re.sub ('\\\\property *[^ .]*[.]?([a-z]+)VerticalDirection[^=]*= *#?"?(1|(\\\\up))"?', '\\\\\\1Up', str)
		str = re.sub ('\\\\property *[^ .]*[.]?([a-z]+)VerticalDirection[^=]*= *#?"?((-1)|(\\\\down))"?', '\\\\\\1Down', str)
		str = re.sub ('\\\\property *[^ .]*[.]?([a-z]+)VerticalDirection[^=]*= *#?"?(0|(\\\\center))"?', '\\\\\\1Both', str)

		# (lacks capitalisation slur -> Slur)
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
	
	conversions.append (((1,3,93), conv,
                'property definiton case (eg. onevoice -> oneVoice)'))


if 1:
	def conv (str):
		str = re.sub ('ChordNames*', 'ChordNames', str)
		if re.search ('\\\\textscript "[^"]* *"[^"]*"', str):
			sys.stderr.write ('\nNot smart enough to convert to new \\textscript markup text')

		str = re.sub ('\\textscript +("[^"]*")', '\\textscript #\\1', str)

		return str
	
	conversions.append (((1,3,97), conv, 'ChordName -> ChordNames'))


# TODO: add lots of these
	
if 1:
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
	
	conversions.append (((1,3,98), conv, 'CONTEXT.textStyle -> GROB.#font-style '))

if 1:
	def conv (str):
		str = re.sub ('"?beamAutoEnd_([0-9]*)"? *= *(#\\([^)]*\\))', 'autoBeamSettings \\push #\'(end 1 \\1 * *) = \\2', str)
		str = re.sub ('"?beamAutoBegin_([0-9]*)"? *= *(#\\([^)]*\))', 'autoBeamSettings \\push #\'(begin 1 \\1 * *) = \\2', str)
		str = re.sub ('"?beamAutoEnd"? *= *(#\\([^)]*\\))', 'autoBeamSettings \\push #\'(end * * * *) = \\1', str)
		str = re.sub ('"?beamAutoBegin"? *= *(#\\([^)]*\\))', 'autoBeamSettings \\push #\'(begin * * * *) = \\1', str)


		return str
	
	conversions.append (((1,3,102), conv, 'beamAutoEnd -> autoBeamSettings \\push (end * * * *)'))


if 1:
	def conv (str):
		str = re.sub ('\\\\push', '\\\\override', str)
		str = re.sub ('\\\\pop', '\\\\revert', str)

		return str
	
	conversions.append (((1,3,111), conv, '\\push -> \\override, \\pop -> \\revert'))

if 1:
	def conv (str):
		str = re.sub ('LyricVoice', 'LyricsVoice', str)
		# old fix
		str = re.sub ('Chord[Nn]ames*.Chord[Nn]ames*', 'ChordNames.ChordName', str)
		str = re.sub ('Chord[Nn]ames([ \t\n]+\\\\override)', 'ChordName\\1', str)
		return str
	
	conversions.append (((1,3,113), conv, 'LyricVoice -> LyricsVoice'))

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
			x = string.upper (x)
		s = s + x
		lastx = x
	return s

if 1:
	def conv (str):
		
		def regularize_dollar_reference (match):
			return regularize_id (match.group (1))
		def regularize_assignment (match):
			return '\n' + regularize_id (match.group (1)) + ' = '
		str = re.sub ('\$([^\t\n ]+)', regularize_dollar_reference, str)
		str = re.sub ('\n([^ \t\n]+)[ \t]*= *', regularize_assignment, str)
		return str
	
	conversions.append (((1,3,117), conv, 'identifier names: $!foo_bar_123 -> xfooBarABC'))


if 1:
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
	
	conversions.append (((1,3,120), conv, 'paper_xxx -> paperXxxx, pedalup -> pedalUp.'))

if 1:
	def conv (str):
		str = re.sub ('drarnChords', 'chordChanges', str)
		str = re.sub ('\\musicalpitch', '\\pitch', str)
		return str
	
	conversions.append (((1,3,122), conv, 'drarnChords -> chordChanges, \\musicalpitch -> \\pitch'))

if 1:
	def conv (str):
		str = re.sub ('ly-([sg])et-elt-property', 'ly-\\1et-grob-property', str)
		return str
	
	conversions.append (((1,3,136), conv, 'ly-X-elt-property -> ly-X-grob-property'))

if 1:
	def conv (str):
		str = re.sub ('point-and-click +#t', 'point-and-click line-column-location', str)
		return str
	
	conversions.append (((1,3,138), conv, 'point-and-click argument changed to procedure.'))

if 1:
	def conv (str):
		str = re.sub ('followThread', 'followVoice', str)
		str = re.sub ('Thread.FollowThread', 'Voice.VoiceFollower', str)
		str = re.sub ('FollowThread', 'VoiceFollower', str)
		return str
	
	conversions.append (((1,3,138), conv, 'followThread -> followVoice.'))

if 1:
	def conv (str):
		str = re.sub ('font-point-size', 'font-design-size', str)
		return str
	
	conversions.append (((1,3,139), conv, 'font-point-size -> font-design-size.'))

if 1:
	def conv (str):
		str = re.sub ('([a-zA-Z]*)NoDots', '\\1Solid', str)
		return str
	
	conversions.append (((1,3,141), conv, 'xNoDots -> xSolid'))

if 1:
	def conv (str):
		str = re.sub ('([Cc])hord([ea])', '\\1ord\\2', str)
		return str
	
	conversions.append (((1,3,144), conv, 'Chorda -> Corda'))


if 1:
	def conv (str):
		str = re.sub ('([A-Za-z]+)MinimumVerticalExtent', 'MinimumV@rticalExtent', str)
		str = re.sub ('([A-Za-z]+)ExtraVerticalExtent', 'ExtraV@rticalExtent', str)
		str = re.sub ('([A-Za-z]+)VerticalExtent', 'VerticalExtent', str)
		str = re.sub ('ExtraV@rticalExtent', 'ExtraVerticalExtent', str)
		str = re.sub ('MinimumV@rticalExtent', 'MinimumVerticalExtent', str)		
		return str

	conversions.append (((1,3,145), conv,
	'ContextNameXxxxVerticalExtent -> XxxxVerticalExtent'))

if 1:
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
	conversions.append (((1,3,146), conv, 'semicolons removed'))

if 1:
	def conv (str):
		str = re.sub ('default-neutral-direction', 'neutral-direction',str)
		return str
	conversions.append (((1,3,147), conv, 'default-neutral-direction -> neutral-direction'))

if 1:
	def conv (str):
		str = re.sub ('\(align', '(axis', str)
		str = re.sub ('\(rows', '(columns', str)
		return str
	conversions.append (((1,3,148), conv, '"(align" -> "(axis", "(rows" -> "(columns"'))


if 1:
	def conv (str):
		str = re.sub ('SystemStartDelimiter', 'systemStartDelimiter', str)
		return str
	conversions.append (((1,5,33), conv, 'SystemStartDelimiter -> systemStartDelimiter'))

if 1:
	def conv (str):
		str = re.sub ('arithmetic-multiplier', 'spacing-increment', str)
		str = re.sub ('arithmetic-basicspace', 'shortest-duration-space', str)		
		return str
	
	conversions.append (((1,5,38), conv, 'SystemStartDelimiter -> systemStartDelimiter'))


if 1:
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

	# 40 ?
	conversions.append (((1,5,40), conv, 'breakAlignOrder property names'))
	

if 1:
	def conv (str):
		str = re.sub ('noAutoBeaming *= *##f', 'autoBeaming = ##t', str)
		str = re.sub ('noAutoBeaming *= *##t', 'autoBeaming = ##f', str)
		return str
	
	conversions.append (((1,5,49), conv, 'noAutoBeaming -> autoBeaming'))

if 1:
	def conv (str):
		str = re.sub ('tuplet-bracket-visibility', 'bracket-visibility', str)
		str = re.sub ('tuplet-number-visibility', 'number-visibility', str)		
		return str
	
	conversions.append (((1,5,52), conv, 'tuplet-X-visibility -> X-visibility'))

if 1:
	def conv (str):
		str = re.sub ('Pitch::transpose', 'ly-transpose-pitch', str)

		return str
	
	conversions.append (((1,5,56), conv, 'Pitch::transpose -> ly-transpose-pitch'))

if 1:
	def conv (str):
		str = re.sub ('textNonEmpty *= *##t', "TextScript \\set #'no-spacing-rods = ##f", str)
		str = re.sub ('textNonEmpty *= *##f', "TextScript \\set #'no-spacing-rods = ##t", str)
		return str
	
	conversions.append (((1,5,58), conv, 'deprecate textNonEmpty'))


if 1:
	def conv (str):
		str = re.sub ('MinimumVerticalExtent', 'minimumV@rticalExtent', str)
		str = re.sub ('minimumVerticalExtent', 'minimumV@rticalExtent', str)		
		str = re.sub ('ExtraVerticalExtent', 'extraV@rticalExtent', str)
		str = re.sub ('extraVerticalExtent', 'extraV@rticalExtent', str)		
		str = re.sub ('VerticalExtent', 'verticalExtent', str)
		str = re.sub ('extraV@rticalExtent', 'extraVerticalExtent', str)
		str = re.sub ('minimumV@rticalExtent', 'minimumVerticalExtent', str)		
		return str

	conversions.append (((1,5,59), conv,
	'XxxxVerticalExtent -> xxxVerticalExtent'))

if 1:
	def conv (str):
		str = re.sub ('visibility-lambda', 'break-visibility', str)
		return str

	conversions.append (((1,5,62), conv,
	'visibility-lambda -> break-visibility'))
	

if 1:
	def conv (str):
		if re.search (r'\addlyrics',str) \
		       and re.search ('automaticMelismata', str)  == None:
			sys.stderr.write  ('automaticMelismata is turned on by default since 1.5.67. Please fix this by hand.')
			raise FatalConversionError()
		return str

	conversions.append (((1,5,67), conv,
			     'automaticMelismata turned on by default'))

if 1:
	def conv (str):
		str = re.sub ('ly-set-grob-property([^!])', 'ly-set-grob-property!\1', str)
		str = re.sub ('ly-set-mus-property([^!])', 'ly-set-mus-property!\1', str)		
		return str
	
	conversions.append (((1,5,68), conv, 'ly-set-X-property -> ly-set-X-property!'))

if 1:
	def conv (str):
		str = re.sub ('extent-X', 'X-extent', str)
		str = re.sub ('extent-Y', 'Y-extent', str)		
		return str
	
	conversions.append (((1,5,71), conv, 'extent-[XY] -> [XY]-extent'))


if 1:
	def conv (str):
		str = re.sub ("""#\(set! +point-and-click +line-column-location\)""",
			      """#(set-point-and-click! \'line-column)""", str)
		str = re.sub ("""#\(set![ \t]+point-and-click +line-location\)""",
			      '#(set-point-and-click! \'line)', str)
		str = re.sub ('#\(set! +point-and-click +#f\)',
			      '#(set-point-and-click! \'none)', str)
		return str
	
	conversions.append (((1,5,72), conv, 'set! point-and-click -> set-point-and-click!'))


if 1:
	def conv (str):
		str = re.sub ('flag-style', 'stroke-style', str)
		str = re.sub (r"""Stem([ ]+)\\override #'style""", r"""Stem \\override #'flag-style""", str);
		str = re.sub (r"""Stem([ ]+)\\set([ ]+)#'style""", r"""Stem \\set #'flag-style""", str);
		return str
	
	conversions.append (((1,6,5), conv, 'Stems: flag-style -> stroke-style; style -> flag-style'))


if 1:
	def subst_req_name (match):
		return "(make-music-by-name \'%sEvent)" % regularize_id (match.group(1))

	def conv (str):
		str = re.sub ('\\(ly-make-music *\"([A-Z][a-z_]+)_req\"\\)', subst_req_name, str)
		str = re.sub ('Request_chord', 'EventChord', str)
		return str
	
	conversions.append (((1,7,1), conv, 'ly-make-music foo_bar_req -> make-music-by-name FooBarEvent'))


if 1:
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
		
	def conv (str):
		str = re.sub (r' *= *\\spanrequest *([^ ]+) *"([^"]+)"', subst_definition_ev_name, str)
		str = re.sub (r'\\spanrequest *([^ ]+) *"([^"]+)"', subst_inline_ev_name, str)
		str = re.sub (r' *= *\\commandspanrequest *([^ ]+) *"([^"]+)"', subst_csp_definition, str)
		str = re.sub (r'\\commandspanrequest *([^ ]+) *"([^"]+)"', subst_csp_inline, str)
		str = re.sub (r'ly-id ', 'ly-import ', str)

		str = re.sub (r' *= *\\script "([^"]+)"', ' = #(make-articulation "\\1")', str)
		str = re.sub (r'\\script "([^"]+)"', '#(ly-export (make-articulation "\\1"))', str)
		return str

	conversions.append (((1,7,2), conv, '\\spanrequest -> #(make-span-event .. ), \script -> #(make-articulation .. )'))

if 1:
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

		origre = r'\b(%s)' % string.join (changed, '|')
		
		str = re.sub (origre, r'ly:\1',str)
		str = re.sub ('set-point-and-click!', 'set-point-and-click', str)
		
		return str
	
	conversions.append (((1,7,3), conv, 'ly- -> ly:'))

if 1:
	def conv(str):
		if re.search ('new-chords-done',str):
			return str
		
		str = re.sub (r'<<', '< <', str)
		str = re.sub (r'>>', '> >', str)
		return str
	
	conversions.append (((1,7,4), conv, '<< >> -> < <  > >'))

if 1:
	def conv(str):
		str = re.sub (r"\\transpose", r"\\transpose c'", str)
		str = re.sub (r"\\transpose c' *([a-z]+)'", r"\\transpose c \1", str)
		return str
	conversions.append (((1,7,5), conv, '\\transpose TO -> \\transpose FROM  TO'))

if 1:
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

		origstr = string.join (kws, '|')
		str = re.sub (r'([^_^-])\\(%s)\b' % origstr, r'\1-\\\2', str)
		return str
	conversions.append (((1,7,6), conv, 'note\\script -> note-\script'))


if 1:
	def conv(str):
		str = re.sub (r"\\property *ChordNames *\. *ChordName *\\(set|override) *#'style *= *#('[a-z]+)",
			      r"#(set-chord-name-style \2)", str)
		str = re.sub (r"\\property *ChordNames *\. *ChordName *\\revert *#'style",
			      r"", str)
		return str
	conversions.append (((1,7,10), conv, "\property ChordName #'style -> #(set-chord-name-style 'style)"))
	


if 1:
	def conv(str):
		str = re.sub (r"ly:transpose-pitch", "ly:pitch-transpose", str)
		
		return str
	conversions.append (((1,7,11), conv, "transpose-pitch -> pitch-transpose"))

if 1:
	def conv(str):
		str = re.sub (r"ly:get-molecule-extent", "ly:molecule-get-extent", str)
		str = re.sub (r"ly:set-molecule-extent!", "ly:molecule-set-extent!", str)
		str = re.sub (r"ly:add-molecule", "ly:molecule-add", str)
		str = re.sub (r"ly:combine-molecule-at-edge", "ly:molecule-combine-at-edge", str)
		str = re.sub (r"ly:align-to!", "ly:molecule-align-to!", str)
		
		return str
	
	conversions.append (((1,7,13), conv, "ly:XX-molecule-YY -> ly:molecule-XX-YY"))	

if 1:
	def conv(str):
		str = re.sub (r"linewidth *= *-[0-9.]+ *(\\mm|\\cm|\\in|\\pt)?", 'raggedright = ##t', str )
		return str
	
	conversions.append (((1,7,15), conv, "linewidth = -1 -> raggedright = ##t"))	

if 1:
	def conv(str):
		str = re.sub ("divisiomaior",
			      "divisioMaior", str)
		str = re.sub ("divisiominima",
			      "divisioMinima", str)
		str = re.sub ("divisiomaxima",
			      "divisioMaxima", str)
		return str
	
	conversions.append (((1,7,16), conv, "divisiomaior -> divisioMaior"))

if 1:
	def conv(str):
		str = re.sub ("Skip_req_swallow_translator",
			      "Skip_event_swallow_translator", str)
		return str
	
	conversions.append (((1,7,17), conv, "Skip_req  -> Skip_event"))

if 1:
	def conv(str):
		str = re.sub ("groupOpen",
			      "startGroup", str)
		str = re.sub ("groupClose",
			      "stopGroup", str)
		str = re.sub ("#'outer",
			      "#'enclose-bounds", str)

		return str
	
	conversions.append (((1,7,18), conv,
			     """groupOpen/Close  -> start/stopGroup,
			     #'outer  -> #'enclose-bounds
			     """))

if 1:
	def conv(str):
		if re.search( r'\\GraceContext', str):
			sys.stderr.write ("GraceContext has been removed")
			sys.stderr.write ("please use #(add-to-grace-init .. )")
			raise FatalConversionError()

		str = re.sub ('HaraKiriStaffContext', 'RemoveEmptyStaffContext', str)
		return str
	
	conversions.append (((1,7,19), conv,"remove GraceContext"))



if 1:
	def conv(str):
		str = re.sub (
			r"(set|override|revert) *#'type",
			r"\1 #'style",
			str)
		return str
	
	conversions.append (((1,7,22), conv,"#'type -> #'style"))

if 1:
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
	
	conversions.append (((1,7,23), conv,"barNonAuto -> automaticBars"))
	

if 1:
	def conv(str):
		if re.search( r'-(start|stop)Cluster', str):
			sys.stderr.write ("""Cluster syntax has been changed.
Please refer to the manual for details, and convert manually.
""")
			
			raise FatalConversionError()

		return str
	
	conversions.append (((1,7,24), conv,"cluster syntax"))

if 1:
	def conv(str):
		str = re.sub (r"\\property *Staff\.(Sustain|Sostenuto|UnaCorda)Pedal *\\(override|set) *#'pedal-type *",
				r"\property Staff.pedal\1Style ", str)
		str = re.sub (r"\\property *Staff\.(Sustain|Sostenuto|UnaCorda)Pedal *\\revert *#'pedal-type", '', str)
		return str
	
	conversions.append (((1,7,28), conv,"new Pedal style syntax"))



if 1:

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

		suffix = string.join (slur_strs, '') + string.join (pslur_strs,
								    '') \
			 + string.join (dyns, '')

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

	def text_markup (str):
		str = re.sub (r"""([-_^]) *# *' *\( *music *(\"[^"]*\") *\)""",
				r"\1\\markup { \\musicglyph #\2 }", str)
		str = re.sub (r"""([-_^]) *# *' *\( *([a-z]+) *([^()]*)\)""",
				r"\1\\markup { \\\2 \3 }", str)
		str = re.sub (r"""\\mark *# *' *\( *music *(\"[^"]*\") *\)""",
				r"\\mark \\markup { \\musicglyph #\1 }", str)
		str = re.sub (r"""\\mark *# *' *\( *([a-z]+) *([^()]*)\)""",
				r"\\mark \\markup { \\\1 \2 }", str)
		return str	

	def articulation_substitute (str):
		str = re.sub (r"""([^-])\[ *([a-z]+[,']*[!?]?[0-9:]*\.*)""",
			      r"\1 \2[", str)
		str = re.sub (r"""([^-])\\\) *([a-z]+[,']*[!?]?[0-9:]*\.*)""",
			      r"\1 \2\\)", str)
		str = re.sub (r"""([^-\\])\) *([a-z]+[,']*[!?]?[0-9:]*\.*)""",
			      r"\1 \2)", str)
		str = re.sub (r"""([^-])\\! *([a-z]+[,']*[!?]?[0-9:]*\.*)""",
			      r"\1 \2\\!", str)
		return str
	
	def conv_relative(str):
		if re.search (r"\\relative", str):
			str= "#(ly:set-option 'old-relative)\n" + str

		return str
	
	def conv (str):
		str = re.sub (r"#'\(\)", "@SCM_EOL@", str)
		str =  conv_relative (str)
		str = sub_chords (str)

		str = text_markup (str)
		str = articulation_substitute (str)
		str = re.sub ("@SCM_EOL@", "#'()", str)
		
		return str
	
	conversions.append (((1,9,0), conv, """New relative mode,
Postfix articulations, new text markup syntax, new chord syntax."""))

if 1:
	def conv (str):
		if re.search ("font-style",str):
			sys.stderr.write ("font-style is deprecated. Please remove.")
			raise FatalConversionError()
			
		str = re.sub (r'-\\markup', r'@\\markup', str)
		str = re.sub (r'-\\', r'\\', str)
		str = re.sub (r'-\)', ')', str)
		str = re.sub (r'-\(', '(', str)
		str = re.sub ('-\[', '[', str)
		str = re.sub ('-\]', ']', str)
		str = re.sub ('-~', '~', str)
		str = re.sub (r'@\\markup', r'-\\markup', str)
		return str

	conversions.append (((1,9,1), conv, """Remove - before articulation"""))
if 1:
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
	
	conversions.append (((1,9,2), conv, """\\newcontext -> \\new"""))

if 1:
	def conv (str):
		str = re.sub ('accacciatura',
			      'acciaccatura', str)

		if re.search ("context-spec-music", str):
			sys.stderr.write ("context-spec-music takes a symbol for the context now. Update by hand.")
					
			raise FatalConversionError()
		
		str = re.sub ('fingerHorizontalDirection *= *#(LEFT|-1)',
			      "fingeringOrientations = #'(up down left)", str)
		str = re.sub ('fingerHorizontalDirection *= *#(RIGHT|1)',
			      "fingeringOrientations = #'(up down right)", str)

		return str
	
	conversions.append (((1,9,3), conv,
			     """\\acciaccatura misspelling, fingerHorizontalDirection -> fingeringOrientations"""))


def conv (str):
	if re.search ('\\figures', str):
		sys.stderr.write ("Warning: attempting automatic \\figures conversion.  Check results!");
		
	
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

conversions.append (((1,9,4), conv, 'Swap < > and << >>'))


def conv (str):
	str = re.sub ('HaraKiriVerticalGroup', 'RemoveEmptyVerticalGroup', str)

	return str

conversions.append (((1,9,5), conv, 'HaraKiriVerticalGroup -> RemoveEmptyVerticalGroup'))

def conv (str):
	if re.search ("ly:get-font", str) :
		sys.stderr.write (r"(ly:get-font foo ..)  has been replaced by" + \
				  " (ly:paper-get-font (ly:grob-get-paper foo) .. ).\n" +\
				  "please update manually.")
		
		raise FatalConversionError()
	
	if re.search ("\\pitch *#", str) :
		sys.stderr.write (r"\\pitch has been deprecated. " +\
				  " Use Scheme code to construct arbitrary note events.")
		
		raise FatalConversionError()
	
	return str
		

conversions.append (((1,9,6), conv, 'ly:get-font deprecated.'))

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
		sys.stderr.write (\
			r"""\outputproperty found,
Please hand-edit, using

  \applyoutput #(outputproperty-compatibility %s '%s <GROB PROPERTY VALUE>)

as a substitution text.""" % (m.group (1), m.group (2)) )
		raise FatalConversionError ()

	if re.search ("ly:(make-pitch|pitch-alteration)", str) \
	       or re.search ("keySignature", str):
		sys.stderr.write (
"""The alteration field of Scheme pitches was multiplied by 2
to support quarter tone accidentals. You have to edit the following constructs by hand:

* calls of  ly:make-pitch and ly:pitch-alteration
* keySignature settings made with \property
""")
		raise FatalConversionError ()
	
	return str
conversions.append (((1,9,7), conv,
		     '''use symbolic constants for alterations,
remove \\outputproperty, move ly:verbose into ly:get-option'''))


def conv (str):
	if re.search ("dash-length",str):
		sys.stderr.write ("""dash-length has been removed. Use dash-fraction instead.""")
		raise FatalConversionError()
	return str

conversions.append (((1,9,8), conv, """dash-length -> dash-fraction"""))


def conv (str):
	def func(match):
		return "#'font-size = #%d" % (2*string.atoi (match.group (1))) 
		
	str =re.sub (r"#'font-relative-size\s*=\s*#\+?([0-9-]+)", func, str)
	str =re.sub (r"#'font-family\s*=\s*#'ancient",
		     r"#'font-family = #'music", str)
	
	return str

conversions.append (((2,1,1), conv, """font-relative-size -> font-size"""))

def conv (str):
	str =re.sub (r"ly:get-music-length", "ly:music-length", str)
	return str

conversions.append (((2,1,2), conv, """ly:get-music-length -> ly:music-length"""))

def conv (str):
	str =re.sub (r"\.\s+stz=", ". instr ", str)
	return str

conversions.append (((2,1,3), conv, """stanza -> instrument"""))

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
		
	str =re.sub (r"\\property ([a-zA-Z]+)\s*\.\s*automaticMelismata\s*=\s*##([ft])", func, str)
	return str

conversions.append (((2,1,4), conv, """removal of automaticMelismata; use melismaBusyProperties instead."""))



def conv (str):
	str =re.sub (r"\\translator\s+([a-zA-Z]+)", r"\\change \1", str)
	return str

conversions.append (((2,1,7), conv, """\\translator Staff -> \\change Staff"""))

def conv (str):
	str =re.sub (r"\\newaddlyrics", r"\\lyricsto", str)
	return str

conversions.append (((2,1,10), conv, """\\newaddlyrics -> \\lyricsto"""))

def conv (str):
	str = re.sub (r'\\include\s*"paper([0-9]+)(-init)?.ly"',
		      r"#(set-staff-size \1)", str)

	def sub_note (match):
		dur = ''
		log = string.atoi (match.group (1))
		dots = string.atoi (match.group (2))
		
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

conversions.append (((2,1,11), conv, """\\include "paper16.ly" -> #(set-staff-size 16)
\\note #3 #1 #1 -> \\note #"8." #1
"""))


def conv (str):
	str =re.sub (r"OttavaSpanner", r"OttavaBracket", str)
	return str

conversions.append (((2,1,12), conv, """OttavaSpanner -> OttavaBracket"""))


def conv (str):
	str =re.sub (r"\(set-staff-size ", r"(set-global-staff-size ", str)
	return str

conversions.append (((2,1,13), conv, """set-staff-size -> set-global-staff-size"""))

def conv (str):
	str =re.sub (r"#'style\s*=\s*#'dotted-line",
		     r"#'dash-fraction = #0.0 ", str)
	return str

conversions.append (((2,1,14), conv, """style = dotted -> dash-fraction = 0"""))

def conv (str):
	str =re.sub (r'LyricsVoice\s*\.\s*instrument\s*=\s*("[^"]*")',
		     r'LyricsVoice . vocalName = \1', str)
	
	str =re.sub (r'LyricsVoice\s*\.\s*instr\s*=\s*("[^"]*")',
		     r'LyricsVoice . vocNam = \1', str)
	return str

conversions.append (((2,1,15), conv, """LyricsVoice . instr(ument) -> vocalName"""))

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

conversions.append (((2,1,16), conv, """\\musicglyph #"accidentals-NUM" -> \\sharp/flat/etc."""))


def conv (str):

	if re.search (r'\\partcombine', str):
		sys.stderr.write ('Warning: \\partcombine has been changed. '
				  +'Check conversion manually!')

		raise FatalConversionError()

	# this rule doesn't really work,
	# too lazy to figure out why.
	str = re.sub (r'\\context\s+Voice\s*=\s*one\s*\\partcombine\s+Voice\s*\\context\s+Thread\s*=\s*one(.*)\s*'
		      + r'\\context\s+Thread\s*=\s*two',
		      '\\\\newpartcombine\n\\1\n', str)
	
	
	return str

conversions.append (((2,1,17), conv, """\\partcombine syntax change to \\newpartcombine"""))


def conv (str):
	str = re.sub (r'\\newpartcombine', r'\\partcombine', str)
	str = re.sub (r'\\autochange\s+Staff', r'\\autochange ', str)
	return str

conversions.append (((2,1,18), conv, """\\newpartcombine -> \\partcombine,
\\autochange Staff -> \\autochange
"""))




def conv (str):
	str = re.sub (r'\\include "drumpitch-init.ly"','', str)
	str = re.sub (r'\\pitchnames ','pitchnames = ', str)
	str = re.sub (r'\\chordmodifiers ','chordmodifiers = ', str)
	str = re.sub (r'\bdrums\b\s*=','drumContents = ', str)
	str = re.sub (r'\\drums\b','\\drumContents ', str)
	

	if re.search ('drums->paper', str):
		sys.stderr.write ("\nDrum notation found. Check file manually!")
		
	str = re.sub (r"""\\apply\s+#\(drums->paper\s+'([a-z]+)\)""",
		      r"""\property DrumStaff.drumStyleTable = #\1-style""",
		      str)

	if re.search ('Thread', str):
		sys.stderr.write ("\nThread found. Check file manually!\n");

	str = re.sub (r"""(\\once\s*)?\\property\s+Thread\s*\.\s*NoteHead\s*"""
		      + r"""\\(set|override)\s*#'style\s*=\s*#'harmonic"""
		      + r"""\s+([a-z]+[,'=]*)([0-9]*\.*)"""		      
		      ,r"""<\3\\harmonic>\4""", str)

	str = re.sub (r"""\\new Thread""", """\context Voice""", str)
	str = re.sub (r"""Thread""", """Voice""", str)

	if re.search ('\bLyrics\b', str):
		sys.stderr.write ("\nLyrics found. Check file manually!\n");

	str = re.sub (r"""LyricsVoice""", r"""L@ricsVoice""", str)
	str = re.sub (r"""\bLyrics\b""", r"""LyricsVoice""", str)
	str = re.sub (r"""LyricsContext""", r"""LyricsVoiceContext""", str)
	str = re.sub (r"""L@ricsVoice""", r"""LyricsVoice""",str)
	
	
	return str

conversions.append (((2,1,19), conv, """Drum notation changes, Removing \\chordmodifiers, \\notenames.
Harmonic notes. Thread context removed. Lyrics context removed."""))

def conv (str):
	str = re.sub (r'nonevent-skip', 'skip-music', str)
	return str

conversions.append (((2,1,20), conv, """nonevent-skip -> skip-music""" ))

def conv (str):
	str = re.sub (r'molecule-callback', 'print-function', str)
	str = re.sub (r'brew_molecule', 'print', str)
	str = re.sub (r'brew-new-markup-molecule', 'Text_item::print', str)
	str = re.sub (r'LyricsVoice', 'Lyrics', str)
	str = re.sub (r'tupletInvisible',
		      r"TupletBracket \\set #'transparent", str)
#	str = re.sub (r'molecule', 'collage', str)
#molecule -> collage
	str = re.sub (r"\\property\s+[a-zA-Z]+\s*\.\s*[a-zA-Z]+\s*"
		      + r"\\set\s*#'X-extent-callback\s*=\s*#Grob::preset_extent",
		      "", str)

	return str

conversions.append (((2,1,21), conv, """molecule-callback -> print-function,
brew_molecule -> print
brew-new-markup-molecule -> Text_item::print
LyricsVoice -> Lyrics
tupletInvisible -> TupletBracket \set #'transparent
Grob::preset_extent removed.
""" ))


def conv (str):
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

conversions.append (((2,1,22), conv, """new syntax for property settings:
	\\set A.B = #C , \\unset A.B
	\\override A.B #C = #D, \\revert A.B #C

"""))

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
	
conversions.append (((2,1,23), conv, """Property setting syntax in \\translator{ }"""))
def conv (str):
	str = re.sub (r'music-list\?', 'ly:music-list?', str)
	str = re.sub (r'\|\s*~', '~ |', str)   
	return str

conversions.append (((2,1,24), conv, """music-list? -> ly:music-list?"""))

def conv (str):
	str = re.sub (r'ly:get-spanner-bound', 'ly:spanner-get-bound', str)
	str = re.sub (r'ly:get-extent', 'ly:grob-extent', str)
	str = re.sub (r'ly:get-system', 'ly:grob-system', str)
	str = re.sub (r'ly:get-original', 'ly:grob-original', str)
	str = re.sub (r'ly:get-parent', 'ly:grob-parent', str)
	str = re.sub (r'ly:get-broken-into', 'ly:spanner-broken-into', str)
	str = re.sub (r'Melisma_engraver', 'Melisma_translator', str)
	if re.search ("ly:get-paper-variable", str):
		sys.stderr.write ('use (ly:paper-lookup (ly:grob-paper ))')
		raise FatalConversionError()

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

conversions.append (((2,1,25), conv, """Scheme grob function renaming"""))


def conv (str):
	str = re.sub ('ly:set-grob-property!', 'ly:grob-set-property!',str)
	str = re.sub ('ly:set-mus-property!', 'ly:music-set-property!',str)	
	str = re.sub ('ly:set-context-property!', 'ly:context-set-property!', str)	
	str = re.sub ('ly:get-grob-property', 'ly:grob-property',str)
	str = re.sub ('ly:get-mus-property', 'ly:music-property',str)
	str = re.sub ('ly:get-context-property', 'ly:context-property',str)
	
	return str

conversions.append (((2,1,26), conv, """More Scheme function renaming"""))

def conv (str):
	def subst (m):
		g = string.atoi (m.group (2))
		o = g / 12
		g -= o * 12
		if g <  0:
			g += 12
			o -= 1


		lower_pitches = filter (lambda x : x <= g, [0, 2, 4, 5, 7, 9, 11, 12])
		s = len (lower_pitches) -1 
		a = g - lower_pitches [-1]


		print s , lower_pitches, g, a, s 
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

conversions.append (((2,1,27), conv, """property transposing -> tuning"""))

def conv (str):
	str = re.sub (r'make-music-by-name', 'make-music', str)
	str = re.sub (r"\\override\s+.*Arpeggio\s+#.print-function\s+=\s+\\arpeggioBracket", r"\\arpeggioBracket", str)
	return str

conversions.append (((2,1,28), conv,
		     """make-music-by-name -> make-music,
new syntax for setting \\arpeggioBracket"""))

def conv (str):
	str = re.sub (r'\\center([^-])', '\\center-align\\1', str)
	str = re.sub (r'\\translator', '\\context', str)
	return str

conversions.append (((2,1,29), conv,
		     '\\center -> \\center-align, \\translator -> \\context'))


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

conversions.append (((2,1,30), conv,
		     '''\\threeq{flat,sharp} -> \\sesqui{flat,sharp}
ly:get-mutable-properties -> ly:mutable-music-properties
centralCPosition -> middleCPosition
ly:unset-context-property -> ly:context-unset-property
ly:translator-find -> ly:context-find
ly:get-stencil-extent -> ly:stencil-extent
'''))


def conv (str):
	str = re.sub (r'\\alias\s*"?Timing"?', '', str)
	return str

conversions.append (((2,1,31), conv,
		     '''remove \\alias Timing'''))

def conv (str):
	str = re.sub (r"(\\set\s+)?(?P<context>(Score\.)?)breakAlignOrder\s*=\s*#'(?P<list>[^\)]+)",
		      r"\n\\override \g<context>BreakAlignment #'break-align-orders = "
		      + "#(make-vector 3 '\g<list>)", str)
		      
	return str

conversions.append (((2,1,33), conv,
		     '''breakAlignOrder -> break-align-orders.'''))

def conv (str):
	str = re.sub (r"\(set-paper-size",
		      "(set-default-paper-size",str)
	return str

conversions.append (((2,1,34), conv,
		     '''set-paper-size -> set-default-paper-size.'''))

def conv (str):
	str = re.sub (r"ly:mutable-music-properties",
		      "ly:music-mutable-properties", str)
	return str

conversions.append (((2,1, 36), conv,
		     '''ly:mutable-music-properties -> ly:music-mutable-properties'''))



def conv (str):
	return str

conversions.append (((2, 2, 0), conv,
		     '''clean up version. '''))

def conv (str):
	return re.sub (r'\\apply\b', r'\\applymusic', str)

conversions.append (((2, 3, 1), conv,
		     '''\\apply -> \\applymusic'''))

def conv (str):
	if re.search ('textheight', str):
		sys.stderr.write("\nWarning: tuning of page layout has changed. See reference manual.\n")
		
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

conversions.append (((2, 3, 2), conv,
		     '''\\FooContext -> \\Foo'''))

def conv (str):
	str = re.sub (r'\\notes\b', '', str)
	
	return str

conversions.append (((2, 3, 4), conv,
		     '''remove \\notes'''))



def conv (str):
	str = re.sub (r'lastpagefill\s*=\s*"?1"', 'raggedlastbottom = ##t', str)
	return str

conversions.append (((2, 3, 6), conv,
		     '''lastpagefill -> raggedlastbottom'''))



def conv (str):
	str = re.sub (r'\\consistsend', '\\consists', str)
	str = re.sub (r'\\lyricsto\s+("?[a-zA-Z]+"?)(\s*\\new Lyrics\s*)?\\lyrics',
		      r'\\lyricsto \1 \2', str)
	return str

conversions.append (((2, 3, 8), conv,
		     '''remove \\consistsend, strip \\lyrics from \\lyricsto.'''))

def conv (str):
	str = re.sub (r'neo_mensural', 'neomensural', str)
	str = re.sub (r'if-text-padding', 'bound-padding', str)
	return str

conversions.append (((2, 3, 9), conv,
		     '''neo_mensural -> neomensural, if-text-padding -> bound-padding'''))



def conv (str):
	str = re.sub (r'\\addlyrics', r'\\oldaddlyrics', str)
	str = re.sub (r'\\newlyrics', r'\\addlyrics', str)
	if re.search (r"\\override\s*TextSpanner", str):
		sys.stderr.write ("\nWarning: TextSpanner has been split into DynamicTextSpanner and TextSpanner\n") 
	return str

conversions.append (((2, 3, 10), conv,
		     '''\\addlyrics -> \\oldaddlyrics, \\newlyrics -> \\addlyrics'''))

def conv (str):
	str = re.sub (r'\\setMmRestFermata\s+(R[0-9.*/]*)',
		      r'\1^\\fermataMarkup', str)
	return str

conversions.append (((2, 3, 11), conv,
		     '''\\setMmRestFermata -> ^\\fermataMarkup'''))

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

	str = re.sub (r'pagenumber\s*=', 'firstpagenumber', str)
	return str

conversions.append (((2, 3, 12), conv,
		     '''\\newpage -> \\pageBreak, junk \\script{up,down,both},
soloADue -> printPartCombineTexts, #notes-to-clusters -> \\makeClusters
'''))


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

conversions.append (((2, 3, 16), conv,
		     '''\foo -> \foomode (for chords, notes, etc.)
fold \new FooContext \foomode into \foo.'''))

def conv (str):
	str = re.sub (r'(slur|stem|phrasingSlur|tie|dynamic|dots|tuplet|arpeggio|)Both', r'\1Neutral', str)
	str = re.sub (r"\\applymusic\s*#\(remove-tag\s*'([a-z-0-9]+)\)",
		      r"\\removeWithTag #'\1", str)
	return str

conversions.append (((2, 3, 17), conv,
		     '''\foo -> \foomode (for chords, notes, etc.)
fold \new FooContext \foomode into \foo.'''))


def conv (str):
	str = re.sub (r'Text_item', 'Text_interface', str)
	return str

conversions.append (((2, 3, 18),
		     conv,
		     '''Text_item -> Text_interface''' ))

def conv (str):
	str = re.sub (r'\\paper', r'\\layout', str)
	str = re.sub (r'\\bookpaper', r'\\paper', str)
	if re.search ('paper-set-staff-size', str):
		sys.stderr.write ('''\nWarning: staff size should be changed at top-level
with

  #(set-global-staff-size <STAFF-HEIGHT-IN-POINT>)

''')
		
		
	str = re.sub (r'#\(paper-set-staff-size', '%Use set-global-staff-size at toplevel\n% #(layout-set-staff-size', str)
	return str

conversions.append (((2, 3, 22),
		     conv,
		     '''paper -> layout
 bookpaper -> paper''' ))


def conv (str):
	str = re.sub (r'\\context\s+([a-zA-Z]+)\s*=\s*([a-z]+)\s',
		      r'\\context \1 = "\2" ',
		      str )
	return str

conversions.append (((2, 3, 23),
		     conv,
		     '''\context Foo = NOTENAME -> \context Foo = "NOTENAME"'''))


def conv (str):
	return str

conversions.append (((2, 4, 0),
		     conv,
		     ''))




################################
#	END OF CONVERSIONS	
################################

def get_conversions (from_version, to_version):
	def version_b (v, f = from_version, t = to_version):
		return version_cmp (v[0], f) > 0 and version_cmp (v[0], t) <= 0
	return filter (version_b, conversions)


def latest_version ():
	return conversions[-1][0]

def do_conversion (infile, from_version, outfile, to_version):
	conv_list = get_conversions (from_version, to_version)

	sys.stderr.write ('Applying conversions: ')
	str = infile.read ()
	last_conversion = ()
	try:
		for x in conv_list:
			sys.stderr.write (tup_to_str (x[0]) + ', ')
			str = x[1] (str)
			last_conversion = x[0]

	except FatalConversionError:
		sys.stderr.write ('Error while converting; I won\'t convert any further')

	if last_conversion:
		sys.stderr.write ('\n')
		new_ver =  '\\version \"%s\"' % tup_to_str (last_conversion)

		if re.search (lilypond_version_re_str, str):
			str = re.sub (lilypond_version_re_str,'\\'+new_ver, str)
		elif add_version:
			str = new_ver + '\n' + str

		outfile.write (str)

	return last_conversion
	
class UnknownVersion:
	pass

def do_one_file (infile_name):
	sys.stderr.write ('Processing `%s\' ... '% infile_name)
	outfile_name = ''
	if __main__.edit:
		outfile_name = infile_name + '.NEW'
	elif __main__.outfile_name:
		outfile_name = __main__.outfile_name

	if __main__.from_version:
		from_version = __main__.from_version
	else:
		guess = guess_lilypond_version (infile_name)
		if not guess:
			raise UnknownVersion ()
		from_version = str_to_tuple (guess)

	if __main__.to_version:
		to_version = __main__.to_version
	else:
		to_version = latest_version ()


	if infile_name:
		infile = open (infile_name,'r')
	else:
		infile = sys.stdin

	if outfile_name:
		outfile =  open (outfile_name, 'w')
	else:
		outfile = sys.stdout

	touched = do_conversion (infile, from_version, outfile, to_version)

	if infile_name:
		infile.close ()

	if outfile_name:
		outfile.close ()

	if __main__.edit and touched:
		try:
			os.remove(infile_name + '~')
		except:
			pass
		os.rename (infile_name, infile_name + '~')
		os.rename (infile_name + '.NEW', infile_name)

	sys.stderr.write ('\n')
	sys.stderr.flush ()

edit = 0
assume_old = 0
to_version = ()
from_version = ()
outfile_name = ''
show_rules_p = 0

(options, files) = getopt.getopt (sys.argv[1:], 'ao:f:t:senh',
				  ['no-version', 'version', 'output',
				  'show-rules', 'help', 'edit',
				  'from=', 'to='])

for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--help' or o == '-h':
		usage ()
		sys.exit (0)
	elif o == '--version' or o == '-v':
		print_version ()
		sys.exit (0)
	elif o== '--from' or o=='-f':
		from_version = str_to_tuple (a)
	elif o== '--to' or o=='-t':
		to_version = str_to_tuple (a)
	elif o== '--edit' or o == '-e':
		edit = 1
	elif o== '--show-rules' or o == '-s':
		show_rules_p = 1
	elif o == '--output' or o == '-o':
		outfile_name = a
	elif o == '--no-version' or o == '-n':
		add_version = 0
	else:
		print o
		raise getopt.error

# should parse files[] to read \version?
if show_rules_p:
	show_rules (sys.stdout)
	sys.exit (0)
		
identify ()
for f in files:
	if f == '-':
		f = ''
	elif not os.path.isfile (f):
		continue
	try:
		do_one_file (f)
	except UnknownVersion:
		sys.stderr.write ('\n')
		sys.stderr.write ("%s: can't determine version for `%s'" % (program_name, f))
		sys.stderr.write ('\n')
		if assume_old:
			fv = from_version
			from_version = (0, 0, 0)
			do_one_file (f)
			from_version = fv
		else:
			sys.stderr.write ("%s: skipping: `%s' " % (program_name,  f))
		pass

sys.stderr.write ('\n')
