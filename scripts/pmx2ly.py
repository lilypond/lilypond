#!@PYTHON@

# (urg! wat een pokkeformaat (pokkenformaat?))  
import os
import string
import sys
import re
import getopt

program_name = 'pmx2ly'
version = '@TOPLEVEL_VERSION@'
if version == '@' + 'TOPLEVEL_VERSION' + '@':
	version = '(unknown version)'	   # uGUHGUHGHGUGH


def encodeint (i):
	return chr ( i  + ord ('A'))

def stripcomment (l):
	return re.sub ('[ \t]*%.*$\n', '', l)
	
def stripwhite (l):
	return re.sub ('[ \n\t]+', ' ', l)
	
def stripeols (l):
	return re.sub ('^ ',  '', re.sub (' $', '', l))
	
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

def gcd (a,b):
	if b == 0:
		return a
	c = a
	while c: 
		c = a % b
		a = b
		b = c
	return a

def rat_simplify (r):
	(n,d) = r
	if d < 0:
		d = -d
		n = -n
	if n == 0:
		return (0,1)
	else:
		g = gcd (n, d)
		return (n/g, d/g)
	
def rat_multiply (a,b):
	(x,y) = a
	(p,q) = b

	return rat_simplify ((x*p, y*q))

def rat_divide (a,b):
	(p,q) = b
	return rat_multiply (a, (q,p))

tuplet_table = {
	2: 3,
	3: 2,
	5: 4
}


def rat_add (a,b):
	(x,y) = a
	(p,q) = b

	return rat_simplify ((x*q + p*y, y*q))

def rat_neg (a):
	(p,q) = a
	return (-p,q)


def rat_larger (a,b):
	return rat_subtract (a, b )[0] > 0

def rat_subtract (a,b ):
	return rat_add (a, rat_neg (b))

def rat_to_duration (frac):
	log = 1
	d = (1,1)
	while rat_larger (d, frac):
		d = rat_multiply (d, (1,2))
		log = log << 1

	frac = rat_subtract (frac, d)
	dots = 0
	if frac == rat_multiply (d, (1,2)):
		dots = 1
	elif frac == rat_multiply (d, (3,4)):
		dots = 2
	return (log, dots)	


class Barcheck :
	def __init__ (self):
		pass
	def dump (self):
		return '|\n'

class Beam:
	def __init__ (self, ch):
		self.char = ch
	def dump (self):
		return self.char

class Slur:
	def __init__ (self,id):
		self.id = id
		self.start_chord = None
		self.end_chord = None
	def calculate (self):
		s =self.start_chord
		e= self.end_chord

		if e and s:
			s.note_suffix = s.note_suffix + '('
			e.note_prefix = ')' + e.note_prefix
		else:
			sys.stderr.write ("\nOrphaned slur")
			
class Voice:
	def __init__ (self):
		self.entries = []
		self.chords = []
		self.staff = None
		self.current_slurs = []
		self.slurs = []
	def toggle_slur (self, id):
		
		for s in self.current_slurs:
			if s.id == id:
				self.current_slurs.remove (s)
				s.end_chord = self.chords[-1]
				return
		s = Slur (id)
		s.start_chord = self.chords[-1]
		self.current_slurs.append (s)
		self.slurs.append (s)
		
	def last_chord (self):
		return self.chords[-1]
	def add_chord (self, ch):
		self.chords.append (ch)
		self.entries.append (ch)
	def add_nonchord (self, nch):
		self.entries.append (nch)

	def idstring (self):
		return 'staff%svoice%s ' % (encodeint (self.staff.number) , encodeint(self.number))
	def dump (self):
		str = ''
		ln = ''
		for e in self.entries:
			next = ' ' + e.dump ()
			if next[-1] == '\n':
				str  = str + ln + next
				ln = ''
				continue
			
			if len (ln) +len (next) > 72:
				str = str+ ln + '\n'
				ln = ''
			ln = ln + next
			
			
		str = str  + ln
		id = self.idstring ()
			
		str = '%s =  \\notes { \n %s }\n '% (id, str)
		return str
	def calculate_graces (self):
		lastgr = 0
		lastc = None
		for c in self.chords:
			if c.grace and  not lastgr:
				c.chord_prefix = c.chord_prefix + '\\grace { '
			elif not c.grace and lastgr:
				lastc.chord_suffix = lastc.chord_suffix + ' } '
			lastgr = c.grace
			lastc = c
	def calculate (self):
		self.calculate_graces ()
		for s in self.slurs:
			s.calculate ()

class Clef:
	def __init__ (self, cl):
		self.type = cl
	def dump(self):
		return '\\clef %s;' % self.type

clef_table = {
	'b':'bass'  ,
	'r':'baritone',
	'n':'tenor',
	'a':'alto',
	'm':'mezzosoprano',
	's':'soprano',
	't':'treble',
	'f':'frenchviolin',
	} 
class Staff:
	def __init__ (self): 
		self.voices = (Voice (), Voice())
		self.clef = None
		self.instrument = 0
		self.voice_idx = 0
		self.number = None
		
		i = 0
		for v  in self.voices:
			v.staff = self
			v.number = i
			i = i+1
	def set_clef (self, letter):

		clstr = clef_table[letter]
		self.voices[0].add_nonchord (Clef (clstr))
		
	def current_voice (self):
		return self.voices[self.voice_idx]
	def next_voice (self):
		self.voice_idx = (self.voice_idx + 1)%len (self.voices)

	def calculate (self):
		for v in self.voices:
			v.calculate ()
	def idstring (self):
		return 'staff%s' % encodeint (self.number)
	def dump (self):
		str = ''

		refs = ''
		for v in self.voices:
			str = str + v.dump()
			refs = refs + '\\' + v.idstring ()+  ' '
		
		str = str + '\n\n%s = \\context Staff = %s \n  < \n %s >\n\n\n'% (self.idstring (), self.idstring (), refs)
		return str

class Tuplet:
	def __init__ (self, number, base, dots):
		self.chords = []
		self.number = number
		self.replaces = tuplet_table[number]
		self.base = base
		self.dots = dots
		
		length = (1,base)
		if dots == 1:
			length = rat_multiply (length, (3,2))
		elif dots == 2:
			length = rat_multiply (length, (7,4))

		length = rat_multiply (length, (1,self.replaces))

		(nb,nd) =rat_to_duration (length)

		self.note_base = nb
		self.note_dots = nd

	def add_chord (self, ch):
		ch.dots = self.note_dots
		ch.basic_duration = self.note_base
		self.chords.append (ch)

		if len (self.chords) == 1:
			ch.chord_prefix = '\\times %d/%d { ' % (self.replaces, self.number)
		elif len (self.chords) == self.number:
			ch.chord_suffix = ' }' 
		
class Chord:
	def __init__ (self):
		self.pitches = []
		self.dots = 0
		self.basic_duration = 0
		self.scripts = []
		self.grace = 0
		self.chord_prefix = ''
		self.chord_suffix = ''
		self.note_prefix = ''
		self.note_suffix = ''
		
	def dump (self):
		str = ''

		sd = ''
		if self.basic_duration == 0.5:
			sd = '\\breve'
		else:
			sd = '%d' % self.basic_duration
		sd = sd + '.' * self.dots 
		for p in self.pitches:
			if str:
				str = str + ' ' 
			str = str + pitch_to_lily_string (p) + sd

		for s in self.scripts:
			str = str + '-' + s

		str = self.note_prefix +str  + self.note_suffix
		
		if len (self.pitches) > 1:
			str = '<%s>' % str
		elif len (self.pitches) == 0:
			str = 'r' + sd

		str = self.chord_prefix + str + self.chord_suffix
		
		return str
		
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


ornament_table = {
	't': '\\prall',
	'm': '\\mordent',
	'x': '"x"',
	'+': '+',
	'u': '"pizz"',
	'p': '|',
	'(': '"paren"',
	')': '"paren"',
	'g': '"segno"',
	'.': '.',
	'fd': '\\fermata',
	'f': '\\fermata',
	'_': '-',
	'T': '\\trill',
	'>': '>',
	'^': '^',
	}

class Parser:
	def __init__ (self, filename):
		self.staffs = []
		self.forced_duration = None
		self.last_name = 0
		self.last_oct = 0		
		self.tuplets_expected = 0
		self.tuplets = []
		self.last_basic_duration = 4

		self.parse (filename)
		
	def set_staffs (self, number):
		self.staffs = map (lambda x: Staff (), range(0, number))
		
		self.staff_idx = 0

		i =0
		for s in self.staffs:
			s.number = i
			i = i+1
	def current_staff (self):
		return self.staffs[self.staff_idx]

	def current_voice (self):
		return self.current_staff ().current_voice ()
	
	def next_staff (self):
		self.staff_idx = (self.staff_idx + 1)% len (self.staffs)
		
	def parse_note (self, str):
		name = None
		ch = None

		grace = 0
		if str[0] == 'G':
			grace = 1
			str = str[1:]
			
		if str[0] == 'z':
			ch = self.current_voice().last_chord()
			str = str[1:]
		else:
			ch = Chord ()
			self.current_voice().add_chord (ch)			
		if str[0] <> 'r':
			name = (ord (str[0]) - ord('a') + 5) % 7

		str = str[1:]

		ch.grace = ch.grace or grace 
		
		forced_duration  = 0
		alteration = 0
		dots = 0
		oct = None
		durdigit = None
		multibar = 0
		tupnumber = 0
		extra_oct = 0
		while str[0] in 'dsfmnul0123456789.,+-':
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
			elif c in DIGITS and durdigit == None and \
			     self.tuplets_expected == 0:
				durdigit = string.atoi (c)
			elif c in DIGITS:
				oct = string.atoi (c) - 3
			elif c == '+':
				extra_oct = extra_oct + 1
			elif c == '-':
				extra_oct = extra_oct - 1
			elif c == '.':
				dots = dots+ 1
				forced_duration = 2
			elif c == ',':
				forced_duration = 2

		if str[0] == 'x':
			str = str[1:]
			tupnumber = string.atoi (str[0])
			str = str[1:]
			str=re.sub (r'^n?f?[+-0-9.]+', '' , str)

		
		if durdigit:
			try:
				basic_duration =  basicdur_table[durdigit]
				self.last_basic_duration = basic_duration
			except KeyError:
				sys.stderr.write ("""
Huh? expected duration, found %d Left was `%s'""" % (durdigit, str[:20]))

				basic_duration = 4
		else:
			basic_duration = self.last_basic_duration


		
		if name <> None and oct == None:
			e = 0
			if self.last_name < name and name -self.last_name > 3:
				e = -1
			elif self.last_name > name and self.last_name -name > 3:
				e = 1

			oct = self.last_oct  +e + extra_oct

		if name <> None:
			self.last_oct = oct
			self.last_name = name
				
		if name <> None:
			ch.pitches.append ((oct, name,  alteration))

		# do before adding to tuplet.
		ch.basic_duration = basic_duration
		ch.dots = dots

		if forced_duration:
			self.forced_duration = ch.basic_duration / forced_duration

		if tupnumber:
			tup =Tuplet (tupnumber, basic_duration, dots)
			self.tuplets_expected = tupnumber
			self.tuplets.append (tup)

		if self.tuplets_expected > 0:
			self.tuplets[-1].add_chord (ch)
			self.tuplets_expected = self.tuplets_expected - 1
			
		return str
	def parse_basso_continuo (self, str):
		while str[0] in DIGITS +'#n-':
			scr = str[0]

			if scr == '#':
				scr = '\\\\textsharp'
			
			if len(scr)>1 or scr not in DIGITS:
				scr = '"%s"' % scr
				
			self.current_voice().last_chord ().scripts.append (scr)
			str=str[1:]
		return str
	def parse_beams (self,str):
		c = str[0]
	#	self.current_voice().add_nonchord (Beam(c))
		if str[0] == '[':
			str = str[1:]
			while str[0] in '+-0123456789':
				str=str[1:]
		else:
			str = str[1:]
					
		return str
	
	def clean (self, ls):
		ls = map (stripcomment, ls)
		ls = map (stripwhite, ls)
		ls = map (stripeols, ls)


		ls = filter (lambda x: x <> '', ls)
		return ls

	def parse_header  (self, ls):

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


		l = ls[0]
		ls = ls[1:]


		self.set_staffs (no_staffs)
		for s in self.staffs:
			s.set_clef(l[0])
			l = l[1:]

		# dump path 
		ls = ls[1:] 

		# dump more ?
		ls = ls[2:]

		return ls

	def parse_ornament (self, left):
		left = left[1:]
		e = self.current_voice ().last_chord ()

		id = left[0]
		left = left[1:]
		if left[0] == 'd':
			id = id +'d'
			left = left [1:]

		orn = '"orn"'
		try:
			orn = ornament_table[id]
		except KeyError:
			sys.stderr.write ("unknown ornament `%s'\n" % id)
			
		e.scripts.append (orn)
		return left
	def parse_barcheck (self, left):
		self.current_voice ().add_nonchord (Barcheck ())
		
		return left [1:]

	def parse_slur (self, left):
		left = left[1:]

		id = None

		if re.match ('[A-Z0-9]', left[0]):
			id = left[0]
			left= left[1:]
		while left[0] in 'uld0123456789+-.':
			left= left[1:]
			
		self.current_voice ().toggle_slur (id)
		return left

	def parse_mumbo_jumbo (self,left):
		left = left[1:]
		while left and  left[0] <> '\\':
			left = left[1:]

		left  = left[1:]
		return left
	def parsex (self,left):
		left = left[1:]
		while left[0] in DIGITS:
			left = left[1:]

		return left
	
	def parse_body (self, left):
		left = re.sub ('[ \t\n]+',   ' ', left)

		while left:
			c = left[0]
			if c in 'Gzabcdefgr':
				left = self.parse_note (left)
			elif c in DIGITS + 'n#-':
				left = self.parse_basso_continuo (left)
			elif c in SPACE:
				left = left[1:]
			elif c == 's':
				left = self.parse_slur (left)
			elif c == '|':
				left = self.parse_barcheck (left)
			elif c == 'o':
				left = self.parse_ornament (left)
			elif c == 'x':
				left = self.parsex (left)
			elif c in "[]":
				left = self.parse_beams (left)
			elif left[:2] == "//":
				self.current_staff().next_voice ()
				left = left[2:]
			elif c == '/':
				self.next_staff ()
				left = left[1:]
			elif c == '\\':
				left = self.parse_mumbo_jumbo(left)
			else:
				sys.stderr.write ("""
Huh? Unknown directive `%s', before `%s'""" % (c, left[:20] ))
				left = left[1:]

		for c in self.staffs:
			c.calculate ()

	def dump (self):
		str = ''

		refs = ''
		for s in self.staffs:
			str = str +  s.dump ()
			refs = '\\' + s.idstring() + refs

		str = str + "\n\n\\score { <\n %s\n > }" % refs 
		return str
			

	def parse (self,fn):
		ls = open (fn).readlines ()
		ls = self.clean (ls)
		ls = self.parse_header (ls)
		left = string.join (ls, ' ')
		self.parse_body (left)
		



def help ():
	sys.stdout.write (
"""Usage: pmx2ly [OPTION]... PMX-FILE

Convert PMX to LilyPond.

Options:
  -h, --help          this help
  -o, --output=FILE   set output filename to FILE
  -v, --version       version information

PMX is a Musixtex preprocessor written by Don Simons, see
http://www.gmd.de/Misc/Music/musixtex/software/pmx/

Report bugs to bug-gnu-music@gnu.org.

Written by Han-Wen Nienhuys <hanwen@cs.uu.nl>
""")


def print_version ():
	sys.stdout.write ("""pmx2ly (GNU LilyPond) %s

This is free software.  It is covered by the GNU General Public License,
and you are welcome to change it and/or distribute copies of it under
certain conditions.  Invoke as `midi2ly --warranty' for more information.

Copyright (c) 2000 by Han-Wen Nienhuys <hanwen@cs.uu.nl>
""" % version)
def identify():
	sys.stderr.write ("%s from LilyPond %s\n" % (program_name, version))



(options, files) = getopt.getopt (sys.argv[1:], 'vo:h', ['help','version', 'output='])
out_filename = None
for opt in options:
	o = opt[0]
	a = opt[1]
	if o== '--help' or o == '-h':
		help ()
		sys.exit (0)
	if o == '--version' or o == '-v':
		print_version ()
		sys.exit(0)
		
	if o == '--output' or o == '-o':
		out_filename = a
	else:
		print o
		raise getopt.error

identify()

for f in files:
	if f == '-':
		f = ''

	sys.stderr.write ('Processing `%s\'\n' % f)
	e = Parser(f)
	if not out_filename:
		out_filename = os.path.basename (re.sub ('(?i).pmx$', '.ly', f))
		
	if out_filename == f:
		out_filename = os.path.basename (f + '.ly')
		
	sys.stderr.write ('Writing `%s\'' % out_filename)
	ly = e.dump()

	
	
	fo = open (out_filename, 'w')
	fo.write ('%% lily was here -- automatically converted by pmx2ly from %s\n' % f)
	fo.write(ly)
	fo.close ()
	

