#!@PYTHON@

# musedata = musedata.stanford.edu
# musedata = COBOL for musicians.


# TODO
#
# * clefs,
# * keys,
# * staffs,
# * multiple voices (they use `Backspace' (shudder)
# * tuplets
#

#
# I completely forgot how this was supposed to work --hwn 5/2002 
#
#

import re
import sys
import string
import getopt
import os
program_name = 'musedata2ly'
version = '@TOPLEVEL_VERSION@'
if version == '@' + 'TOPLEVEL_VERSION' + '@':
	version = '(unknown version)'	   # uGUHGUHGHGUGH



ref_header_dict = {
	'COM': 'composer',
	'OPR': 'collection',
	'OTL': 'title',
	'OMV': 'subtitle',
	'YOR': 'source',
	'AGN': 'instrument',
	'END': 'encodingdate',
	'CDT': 'date',
	'OCY': 'composedin',
	'AST': 'genre',
	'YEC': 'copyright',
	'YEM': 'license',
	'YEN': 'encodingcountry',
	'EED': 'editor',
	'SCA': 'opus',
	'ONM': 'onm',
	'ENC': 'musedataencoder',
	'KEY': 'musedatakey',
	'AFT': 'musedatastage'
	}


class Ref_parser:
	def __init__ (self, fn):
		self.dict = {}
		
		ls = open (fn).readlines ()
		self.parse (ls)
	def parse (self,ls):
		for l in ls:
			m = re.match('!!!([A-Z]+):[ \t]+(.*)$',l)
			if m:
				key = m.group(1)
				val = m.group (2)
				val = re.sub ('[ \t]+', ' ', val)
				try:
					
					key =ref_header_dict [key]
				except KeyError:
					sys.stderr.write ('\nUnknown ref key \`%s\'' % key) 
				s = ''
				try:
					s = self.dict[key]
				except KeyError:
					pass

				s = s + val
				self.dict[key] = s
	def dump( self):
		str = ''
		for (k,v) in self.dict.items ():
			str = str +'  %s = "%s"\n' % (k,v)
		str = '\\header {\n%s}' % str
		return str
	
verbose = 0


actab = {-2: 'eses', -1: 'es', 0 : '', 1: 'is', 2:'isis'}

def pitch_to_lily_string (tup):
	(o,n,a) = tup

	nm = chr((n + 2) % 7 + ord ('a'))
	nm = nm + actab[a]
	if o > 0:
		nm = nm + "'" * o
	elif o < 0:
		nm = nm + "," * -o
	return nm

def get_key (s):
	i = string.atoi (s)
	return ''

def get_timesig (s):
	return '\\time %s\n' % s


divisions = 4
def get_divisions_per_quarter (s):
	divisions = string.atoi (s) 
	return ''

def get_directive (s):
	return '%% %s\n' % s

def get_transposing (s):
	return ''

def get_num_instruments (s):
	return ''

def get_lilypond_notename (p, ac):
	if p > 5:
		p = p - 7
	s = chr (p + ord ('c'))
	infix = 'i'
	if ac < 0:
		infix = 'e'
		ac = -ac

	while ac:
		s = s + infix + 's'
		ac = ac - 1
	return s
def get_clef ():
	return ''

SPACES = ' '
DIGITS = "0123456789"


clef_dict = {
04: 'treble',
13 : 'alto',
22: 'bass',
}
attr_dict = {
	'C' : get_clef,
	'K' : get_key ,
	'T' : get_timesig,
	'Q' : get_divisions_per_quarter,
	'D' : get_directive,
	'X' : get_transposing,
	'I': get_num_instruments,
	}

class Attribute_set:
	def __init__ (self, dict):
		self.dict = dict
	def dump (self):
		s = ''
		if self. dict.has_key ('T'):
			s = s+ get_timesig  (self.dict['T'])
		
		return s


script_table = {
'v': '\\upbow',
'n': '\\downbow',
'o': '\\harmonic',
'0': '"openstring',
'Q': '\\thumb',
'>': '^',
'V': '^',
'.': '.',
'_': '-',
'=': '"det leg"',
'i': '|',
's': '"\\\\textsharp"',
'n': '"\\\\textnatural"',
'b': '"\\\\textflat"',
'F': '\\fermata',
'E': '\\fermata',
}


class Chord:
	def __init__ (self):
		self.pitches = []
		self.grace = 0
		self.cue = 0
		self.slurstart = []
		self.slurstop  = []
		self.scripts = []
		self.syllables = []
		self.dots = 0
		self.basic_duration = 4
		self.tied = 0

		self.note_suffix = self.note_prefix = ''
		self.chord_suffix = self.chord_prefix = ''
		
	def add_script (self,s):
		self.scripts.append (s)
	def set_duration (self, d):
		self.basic_duration = d
	def add_syllable (self, s):
		self.syllables.append (s)
	def add_pitch (self,t):
		self.pitches.append (t)
		
	def dump (self):
		str = ''

		sd = ''
		if self.basic_duration == 0.5:
			sd = '\\breve'
		else:
			sd = '%d' % self.basic_duration

		sd = sd + '.' * self.dots

		str = ')' * len (self.slurstart) + str
		
		for p in self.pitches:
			if str:
				str = str + ' ' 
			str = str + pitch_to_lily_string (p) + sd
		str = str + '(' * len (self.slurstart)
		

		for s in self.scripts:
			str = str + '-' + s

		str = self.note_prefix +str  + self.note_suffix
		
		if len (self.pitches) > 1:
			str = '<%s>' % str
		elif len (self.pitches) == 0:
			str = 'r' + sd

		str = self.chord_prefix + str + self.chord_suffix
		
		return str

class Measure_start:
	def dump (self):
		return ' |\n'
	
class Parser:
	def append_entry (self, e):
		self.entries.append (e)
	def append_chord (self,c ):
		self.chords.append (c)
		self.entries.append (c)
	def last_chord (self):
		return self.chords[-1]
	def __init__ (self, fn):
		self.divs_per_q = 1
		self.header_dict = {
			'tagline' :'automatically converted from Musedata',
			'copyright' : 'all rights reserved -- free for noncommercial use'
			#  musedata license (argh)
			}
		self.entries = []
		self.chords = []

		
		lines = open (fn).readlines ()
		lines = map (lambda x: re.sub ("\r$", '', x), lines)
		lines = self.parse_header (lines)
		lines = self.append_lines (lines)
		str = string.join (lines, '\n')
		lines = re.split ('[\n\r]+', str)
		self.parse_body (lines)
		
	def parse_header (self, lines):
		enter = string.split (lines[3], ' ')
		self.header_dict['enteredby']  = string.join (enter[1:])
		self.header_dict['enteredon'] = enter[0]
		self.header_dict['opus'] = lines[4]
		self.header_dict['source'] = lines[5]
		self.header_dict['title'] = lines[6]
		self.header_dict['subtitle'] = lines[7]
		self.header_dict['instrument']= lines[8]
		self.header_dict['musedatamisc'] =lines[9]
		self.header_dict['musedatagroups'] =lines[10]
		self.header_dict['musedatagroupnumber']=lines[11]
		lines =  lines[12:]
		comment = 0
		while lines:
			if lines[0][0]  == '$':
				break			
			lines = lines[1:]
		return lines
	
	def parse_musical_attributes (self,l):
		atts = re.split('([A-Z][0-9]?):', l)
		atts = atts[1:]
		found = {}
		while len (atts):
			id = atts[0]
			val = atts[1]
			atts = atts[2:]
			found[id] = val

		try:
			self.divs_per_q = string.atoi (found['Q'])
		except KeyError:
			pass
		
		self.append_entry (Attribute_set (found))
	def append_entry (self, e):
		self.entries.append (e)

	def parse_line_comment (self,l):
		pass

	def parse_note_line (self,l):
		ch = None
		if verbose:
			print DIGITS+DIGITS+DIGITS 
			print l
		pi = l[0:5]
		di = l[5:8]
		tied = l[8:9] == '-'

		cue = grace = 0
		if (pi[0] == 'g'):
			grace = 1
			pi = pi[1:]
		elif (pi[0] == 'c'):
			cue = 1
			pi = pi[1:]
		
		if pi[0] == ' ':
			ch = self.last_chord ()
			pi = pi[1:]
		else:
			ch = Chord ()
			self.append_chord (ch)


		ch.cue = ch.cue or cue
		ch.grace = ch.grace or grace

		while pi[0] in SPACES:
			pi = pi[1:]

		if pi[0] <> 'r':
			name =  ((ord (pi[0]) -ord('A')) + 5) % 7
			alter = 0
			pi = pi[1:]
			while pi and pi[0] in '#f':
				if pi[0] == '#':
					alter = alter + 1
				else:
					alter = alter - 1
				pi = pi[1:]

			oct = string.atoi (pi) - 3

			pittup = (oct, name ,alter)
			ch.add_pitch (pittup)

		ch.dots = 0
		
		dot_str = l[17:18]
		if dot_str  == '.':
			ch.dots = 1
		elif dot_str == ':':
			ch.dots = 2

		base_dur = None
		if ch.cue or ch.grace:
			c = di[2]
			if c == '0':
				ch.accaciatura = 1
			elif c == 'A':
				base_dur = 0.5
			else:
				base_dur = 1 << (9 - (ord (c) - ord ('0')))
		else:
			fact  = (1,1)
			if ch.dots == 1:
				fact = (2,3)
			elif ch.dots == 2:
				fact = (4, 7)
			
			base_dur =  (4 * self.divs_per_q* fact[1]) / (string.atoi (di)* fact[0])
			ch.set_duration (base_dur)

		ch.tied = ch.tied or tied 
	
		if l[26:27] == '[':
			ch.start_beam = 1
		elif l[26:27] == ']':
			ch.end_beam = 1


		additional = l[32:44]
		for c in additional:
			if c in '([{z':
				ch.slurstart.append( 0)
				continue
			elif c in ')]}x':
				ch.slurstop.append( 0)
				continue
			
			if c == '*':
				ch.start_tuplet = 1
				continue
			elif c == '!':
				ch.stop_tuplet = 1
				continue

			if c in DIGITS:
				ch.add_script (c)
				continue

			if c == ' ' :
				continue
			
			try:
				scr = script_table[c]
				ch.add_script (scr)
				c = None
			except KeyError:
				sys.stderr.write ("\nFixme: script `%s' not done\n" % c)

		text = l[40:81]
		sylls = string.split (text,'|')

		for syl in sylls:
			ch.add_syllable (syl)

			
	def parse_measure_line (self,l):
		self.append_entry (Measure_start())


	def parse_duration (l):
		s = ''
		while l[0] in '0123456789':
			s = s + l[0]
			l= l[1:]
		print l
		num = string.atoi (s)
		den = 4 * divisions 

		current_dots = 0
		try_dots = [3, 2, 1]
		for d in try_dots:
			f = 1 << d
			multiplier = (2*f-1)
			if num % multiplier == 0 and den % f == 0:
				num = num / multiplier
				den = den / f
				current_dots = current_dots + d

		if num <> 1:
			sys.stderr.write ('huh. Durations left')
		return '%s%s' % (den, '.' * current_dots)
	
	def append_lines (self,ls):
		nls = []
		for l in ls:
			if l[0] == 'a':
				nls[-1] = nls[-1]+l[1:]
			else:
				nls.append(l)
		return nls
	def dump (self):
		s = ''
		ln = ''
		for e in self.entries:
			
			next = ' ' + e.dump()
			if len (ln) + len (next) > 72:
				s = s +ln +  '\n'
				ln = ''
			ln = ln + next
			
		s = s + ln

		s = '\\notes {\n %s \n}' % s
		return s
	
	def parse_body (self,lines):
		comment_switch = 0
		for l in lines:
			if not l:
				continue
			
			c = l[0]
			if c == '&':
				comment_switch = not comment_switch
				continue
			
			if comment_switch:
				continue

			if 0:
				pass
			elif c == '$':
				self.parse_musical_attributes (l)
			elif c == '@':
				self.parse_line_comment (l)
			elif c == '*':
				self.parse_musical_directions (l)
			elif c in 'ABCDEFGr ':
				self.parse_note_line (l)
			elif c == 'm':
				self.parse_measure_line (l)
			elif c == '/':
				break
			elif c in 'PS':
				pass			# ignore sound & print
			else:
				sys.stderr.write ("\nUnrecognized record `%s'\n"%  l)





def help ():
	sys.stdout.write (
"""Usage: musedata2ly [OPTION]... FILE1 [FILE2 ...]

Convert musedata to LilyPond.

Options:
  -h,--help          this help
  -o,--output=FILE   set output filename to FILE
  -v,--version       version information
  -r,--ref=REF       read background information from ref-file REF     

Musedata (http://www.ccarh.org/musedata/) is an electronic library of
classical music scores, currently comprising XXX scores.  The music is
encoded in so-called Musedata format
(http://www.ccarh.org/publications/books/beyondmidi/online/musedata).
musedata2ly converts a set of musedata files to one .ly file, and will
include a \header field if a .ref file is supplied 

This converter is not complete -- this is left to the user as an excercise.

Report bugs to bug-lilypond@gnu.org.

Written by Han-Wen Nienhuys <hanwen@cs.uu.nl>
""")


def print_version ():
	sys.stdout.write ("""musedata2ly (GNU LilyPond) %s

This is free software.  It is covered by the GNU General Public License,
and you are welcome to change it and/or distribute copies of it under
certain conditions.  Invoke as `midi2ly --warranty' for more information.

Copyright (c) 2000--2002 by Han-Wen Nienhuys <hanwen@cs.uu.nl>
""" % version)
def identify():
	sys.stderr.write ("%s from LilyPond %s\n" % (program_name, version))



(options, files) = getopt.getopt (sys.argv[1:], 'r:vo:h', ['verbose', 'ref=', 'help','version', 'output='])
out_filename = None
ref_file = None
for opt in options:
	o = opt[0]
	a = opt[1]
	if o== '--help' or o == '-h':
		help ()
		sys.exit (0)
	elif o == '--version' or o == '-v':
		print_version ()
		sys.exit(0)
	elif o == '--ref' or o == '-r':
		ref_file = a 
	elif o == '--output' or o == '-o':
		out_filename = a
	elif o == '--verbose' :
		verbose = 1
	else:
		print o
		raise getopt.error

identify()



ly = ''


found_ids = ''

for f in files:
	if f == '-':
		f = ''

	sys.stderr.write ('Processing `%s\'\n' % f)
	
	e = Parser(f)

	id = os.path.basename (f)
	id = re.sub ('[^a-zA-Z0-9]', 'x', id)

	def num2let (match):
		return chr (ord (match.group ()) - ord('0') + ord('A'))
		
	id = re.sub ('[0-9]', num2let, id)
	
	id = 'voice%s' % id
	ly =ly + '\n\n%s = \\context Staff = "%s" %s\n\n' % (id, id, e.dump ())

	found_ids = found_ids + '\\%s\n' % id

found_ids = '\n\n\n\\score { < %s > } ' % found_ids 

ly_head = ''
if ref_file:
	head = Ref_parser (ref_file)
	if not out_filename:
		t = ''
		st = ''
		try:
			t = head.dict['title']
			st= head.dict['subtitle']
		except KeyError:
			pass
			
		t = t + '-' +st
		
		t = re.sub ("^ +(.*) +$", r"\1", t)
		t = re.sub ("\\.", '', t)
		out_filename = re.sub ('[^a-zA-Z0-9-]', '-', t)
		out_filename = out_filename+ '.ly'
	ly_head = head.dump ()
	
if not out_filename:
	out_filename = 'musedata.ly'
	
sys.stderr.write ('Writing `%s\'\n' % out_filename)

fo = open (out_filename, 'w')
fo.write ('%% lily was here -- automatically converted by musedata.ly\n')
fo.write(ly_head + ly + found_ids)
fo.close ()

