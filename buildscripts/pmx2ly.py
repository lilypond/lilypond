#!@PYTHON@

# (urg! wat een pokkeformaat (pokkenformaat?))  

import string
import sys
import re

fn = sys.argv[1]

ls = open (fn).readlines ()
def stripcomment (l):
	return re.sub ('[ \t]*%.*$\n', '', l)
	
def stripwhite (l):
	return re.sub ('[ \n\t]+', ' ', l)
	
def stripeols (l):
	return re.sub ('^ ',  '', re.sub (' $', '', l))
	
ls = map (stripcomment, ls)
ls = map (stripwhite, ls)
ls = map (stripeols, ls)


ls = filter (lambda x: x <> '', ls)

opening = ls[0]
ls = ls[1:]


opening = map (string.atoi, re.split ('[\t ]+', opening))

(no_staffs, no_instruments, timesig_num,timesig_den, ptimesig_num,
 ptimesig_den, pickup_beats,keysig_number) = tuple (opening)


opening = ls[0]
ls = ls[1:]

# ignore this.
# opening = map (string.atoi, re.split ('[\t ]+', opening))
# (no_pages,no_systems, musicsize, fracindent) = tuple (opening)

instruments = []
while len (instruments) < no_instruments:
	instruments.append (ls[0])
	ls = ls[1:]

class Staff:
	def __init__ (self): 
		self.voices = ([],[])
		self.clef = None
		self.instrument = 0 
l = ls[0]
ls = ls[1:]

staffs = map (lambda x: Staff (), range(0, no_staffs))
staff_idx = 0

for s in staffs:
	s.clef = l[0]
	l = l[1:]

# dump path 
ls = ls[1:] 

# dump more ?
ls = ls[2:]

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

class Chord:
	def __init__ (self):
		self.pitches = []
		self.dots = 0
		self.basic_duration = 0
		
	def dump (self):
		str = ''

		for p in self.pitches:
			if str:
				str = str + ' ' 
			str = str + pitch_to_lily_string (p)

		if len (self.pitches) > 1:
			str = '<%s>' % str
		elif len (self.pitches) == 0:
			str = 'r'
		
		
		sd = ''
		if self.basic_duration == 0.5:
			sd = '\\breve'
		else:
			sd = '%d' % self.basic_duration

		str = str + sd + '.' * self.dots 
		return str
		

input_left = string.join (ls, ' ')


input_left = re.sub ('[ \t\n]+',   ' ', input_left)

SPACE=' \t\n'
DIGITS ='0123456789'
basicdur_table = {
	9: 0.5,
	0: 0 ,
	2: 2 ,
	4: 4 ,
	8: 8 ,
	1: 16,
	3: 32,
	6: 64
	}

class Parser:
	def __init__ (self):
		self.chords = []
		self.forced_duration = None
		self.last_octave = 4
		
	def parse_note (self, str):
		ch = Chord ()

		name = None
		if str[0] <> 'r':
			name = (ord (str[0]) - ord('a') + 5) % 7
		str = str[1:]
		
		forced_duration  = 0
		alteration = 0
		dots = 0
		oct = None
		durdigit = None
		multibar = 0
		while str[0] in 'dsfmnul0123456789.,':
			c = str[0]
			str = str[1:]
			if c == 'f':
				alteration = alteration -1
			elif c == 'n':
				alteration = 0
			elif c == 'm':
				multibar = 1
			elif c == 's':
				alteration = alteration +1
			elif c == 'd':
				dots = dots + 1
			elif c in DIGITS and durdigit == None:
				durdigit = string.atoi (c)
			elif c in DIGITS:
				oct = string.atoi (c) - 4
			elif c == '.':
				dots = dots+ 1
				forced_duration = 2
			elif c == ',':
				forced_duration = 2
			

		if durdigit:
			ch.basic_duration = basicdur_table[durdigit]
			self.last_basic_duration = ch.basic_duration
		else:
			ch.basic_duration = self.last_basic_duration

		if name:
			if oct:
				self.last_octave =oct
			else:
				oct = self.last_octave

		if name:
			ch.pitches.append ((oct, name,  alteration))
			
		ch.dots = dots

		
		if forced_duration:
			self.forced_duration = ch.basic_duration / forced_duration


		self.chords.append (ch)
		while str[0] in SPACE:
			str = str [1:]
		return str


parser =  Parser()
while input_left:
	while input_left[0] in 'abcdefgr':
		input_left = parser.parse_note (input_left)
	print input_left[0]
	
	sys.stderr.write ("\nHuh? Unknown directive %s" %input_left[0:1])
	input_left = input_left[1:]



for c in parser.chords:
	print c.dump ()
	
