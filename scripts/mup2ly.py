#!@PYTHON@
# mup2ly.py -- mup input converter
# 
# source file of the GNU LilyPond music typesetter
#
# (c) 2001

'''
TODO:

   WIP:lots of stuff
   
'''

import os
import fnmatch
import stat
import string
import re
import getopt
import sys
import __main__
import operator
import tempfile


sys.path.append ('@datadir@/python')
import gettext
gettext.bindtextdomain ('lilypond', '@localedir@')
gettext.textdomain('lilypond')
_ = gettext.gettext


program_name = 'mup2ly'
help_summary = _("Convert mup to ly")
output = 0

# lily_py.py -- options and stuff
# 
# source file of the GNU LilyPond music typesetter

# BEGIN Library for these?
# cut-n-paste from ly2dvi

program_version = '@TOPLEVEL_VERSION@'
if program_version == '@' + 'TOPLEVEL_VERSION' + '@':
	program_version = '1.3.142'


original_dir = os.getcwd ()
temp_dir = '%s.dir' % program_name
keep_temp_dir_p = 0
verbose_p = 0

def identify ():
	sys.stdout.write ('%s (GNU LilyPond) %s\n' % (program_name, program_version))

def warranty ():
	identify ()
	sys.stdout.write ('\n')
	sys.stdout.write (_ ('Copyright (c) %s by' % ' 2001'))
	sys.stdout.write ('\n')
	sys.stdout.write ('  Han-Wen Nienhuys')
	sys.stdout.write ('  Jan Nieuwenhuizen')
	sys.stdout.write ('\n')
	sys.stdout.write (_ (r'''
Distributed under terms of the GNU General Public License. It comes with
NO WARRANTY.'''))
	sys.stdout.write ('\n')

def progress (s):
        if s[-1] != '\n':
                s = s + '\n'
	sys.stderr.write (s)

def warning (s):
	sys.stderr.write (_ ("warning: ") + s)
	sys.stderr.write ('\n')
	
		
def error (s):
	sys.stderr.write (_ ("error: ") + s)
	sys.stderr.write ('\n')
	raise _ ("Exiting ... ")

def getopt_args (opts):
	'''Construct arguments (LONG, SHORT) for getopt from  list of options.'''
	short = ''
	long = []
	for o in opts:
		if o[1]:
			short = short + o[1]
			if o[0]:
				short = short + ':'
		if o[2]:
			l = o[2]
			if o[0]:
				l = l + '='
			long.append (l)
	return (short, long)

def option_help_str (o):
	'''Transform one option description (4-tuple ) into neatly formatted string'''
	sh = '  '	
	if o[1]:
		sh = '-%s' % o[1]

	sep = ' '
	if o[1] and o[2]:
		sep = ','
		
	long = ''
	if o[2]:
		long= '--%s' % o[2]

	arg = ''
	if o[0]:
		if o[2]:
			arg = '='
		arg = arg + o[0]
	return '  ' + sh + sep + long + arg


def options_help_str (opts):
	'''Convert a list of options into a neatly formatted string'''
	w = 0
	strs =[]
	helps = []

	for o in opts:
		s = option_help_str (o)
		strs.append ((s, o[3]))
		if len (s) > w:
			w = len (s)

	str = ''
	for s in strs:
		str = str + '%s%s%s\n' % (s[0], ' ' * (w - len(s[0])  + 3), s[1])
	return str

def help ():
	sys.stdout.write (_ ("Usage: %s [OPTION]... FILE") % program_name)
	sys.stdout.write ('\n\n')
	sys.stdout.write (help_summary)
	sys.stdout.write ('\n\n')
	sys.stdout.write (_ ("Options:"))
	sys.stdout.write ('\n')
	sys.stdout.write (options_help_str (option_definitions))
	sys.stdout.write ('\n\n')
	sys.stdout.write (_ ("Report bugs to %s") % 'bug-gnu-music@gnu.org')
	sys.stdout.write ('\n')
	sys.exit (0)


def setup_temp ():
	global temp_dir
	if not keep_temp_dir_p:
		temp_dir = tempfile.mktemp (program_name)
	try:
		os.mkdir (temp_dir, 0777)
	except OSError:
		pass
		
	
def system (cmd, ignore_error = 0):
	if verbose_p:
		progress (_ ("Invoking `%s\'") % cmd)
	st = os.system (cmd)
	if st:
		msg =  ( _ ("error: ") + _ ("command exited with value %d") % st)
		if ignore_error:
			sys.stderr.write (msg + ' ' + _ ("(ignored)") + ' ')
		else:
			error (msg)

	return st


def cleanup_temp ():
	if not keep_temp_dir_p:
		if verbose_p:
			progress (_ ('Cleaning up `%s\'') % temp_dir)
		system ('rm -rf %s' % temp_dir)


def set_setting (dict, key, val):
	try:
		val = string.atof (val)
	except ValueError:
		#warning (_ ("invalid value: %s") % `val`)
		pass

	try:
		dict[key].append (val)
	except KeyError:
		warning (_ ("no such setting: %s") % `key`)
		dict[key] = [val]

# END Library


#
# PMX cut and paste
#

def encodeint (i):
	return chr (i  + ord ('A'))

	
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


class Meter :
	def __init__ (self,nums):
		self.nums = nums
	def dump (self):
		return ' %{ FIXME: meter change %} '
		
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
	def __init__ (self, n):
                self.number = n
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
                if len (self.chords):
                        return self.chords[-1]
                else:
                        ch = Chord ()
                        ch.basic_duration = 4
                        return ch
                
	def add_chord (self, ch):
		self.chords.append (ch)
		self.entries.append (ch)
                
	def add_nonchord (self, nch):
		self.entries.append (nch)

	def idstring (self):
		return 'staff%svoice%s ' % (encodeint (self.staff.number) , encodeint(self.number))
        
	def dump (self):
		str = ''
                if not self.entries:
                        #return '\n'
                        #ugh ugh
                        return '\n%s = {}\n\n' % self.idstring ()
                ln = '  '
                one_two = ("One", "Two")
                if self.staff.voices [1 - self.number].entries:
                        ln = ln + '\\voice%s\n  ' % one_two[self.number]
		for e in self.entries:
			next = e.dump ()
			if next[-1] == '\n':
				str  = str + ln + next + ' '
				ln = '  '
				continue
			
			if len (ln) +len (next) > 72:
				str = str+ ln + '\n'
				ln = '  '
			ln = ln + next + ' '
			
			
		str = str  + ln
		id = self.idstring ()
			
		str = '''%s = \\notes {
%s
}

'''% (id, str)
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
	def __init__ (self, n):
                # ugh
		self.voices = (Voice (0), Voice (1))
                
		# self.voice_idx = 0
		self.clef = None
		self.instrument = 0
		self.number = n
		
		i = 0
		for v in self.voices:
			v.staff = self
			v.number = i
			i = i+1
                        
	def set_clef (self, letter):
		clstr = clef_table[letter]
		self.voices[0].add_nonchord (Clef (clstr))
		
	#def current_voice (self):
	#	return self.voices[self.voice_idx]
        #
	#def next_voice (self):
	#	self.voice_idx = (self.voice_idx + 1)%len (self.voices)

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
			refs = refs + '\n  \\' + v.idstring ()
		
		str = str + '''
%s = \context Staff = %s <%s
>

''' % (self.idstring (), self.idstring (), refs)
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
		self.multimeasure = 0
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
		elif self.multimeasure:
			str = 'R' + sd
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

# http://www.arkkra.com/doc/uguide/contexts.html

contexts = [
	'header', 
	'footer', 
	'header2', 
	'footer2', 
	'score', 
	'staff',
	'voice',
	'grids', 
	'music',
	] 

class Parser:
	def __init__ (self, filename):
		self.parse_function = self.parse_context_music
		self.staffs = []
                self.current_voices = []
		self.forced_duration = None
		self.last_name = 0
		self.last_oct = 0		
		self.tuplets_expected = 0
		self.tuplets = []
		self.last_basic_duration = 4

		self.parse (filename)
		
	#def set_staffs (self, number):
	#	self.staffs = map (lambda x: Staff (x), range (0, number))
		
	#def current_staff (self):
	#	return self.staffs[self.staff_idx]

	#def current_voice (self):
	#	return self.current_staff ().current_voice ()
	
	#def next_staff (self):
	#	self.staff_idx = (self.staff_idx + 1)% len (self.staffs)
		
        def parse_compound_location (self, line):
                colon = string.index (line, ':')
                s = line[:colon]
                debug (s)
                line = line[colon + 1:]
                debug (line)
                self.current_voices = []
                ##self.current_staffs = []
                map (self.parse_location, string.split (s, '&'))
                return line

        def parse_location (self, line):
		m = re.match ('^([-,0-9]+) *([-,0-9]*)', string.lstrip (line))
                
                def range_list_to_idxs (s):
                        
                        # duh
                        def flatten (l):
                                f = []
                                for i in l:
                                        for j in i:
                                                f.append (j)
                                return f
                                        
                        def range_to_list (s):
                                if string.find (s, '-') >= 0:
                                        debug ('s: ' + s)
                                        l = map (string.lstrip,
                                                 string.split (s, '-'))
                                        r = range (string.atoi (l[0]) - 1,
                                                   string.atoi (l[1]))
                                else:
                                        r = (string.atoi (s) - 1,)
                                return r
                        
                        ranges = string.split (s, ',')
                        l = flatten (map (range_to_list, ranges))
                        l.sort ()
                        return l
                
                staff_idxs = range_list_to_idxs (m.group (1))
                if m.group (2):
                        voice_idxs = range_list_to_idxs (m.group (2))
                else:
                        voice_idxs = [0]
                for s in staff_idxs:
                        while s > len (self.staffs) - 1:
                                self.staffs.append (Staff (s))
                        for v in voice_idxs:
                                self.current_voices.append (self.staffs[s].voices[v])
                        
	def parse_note (self, line):
                # FIXME: 1?
                oct = 1
                name = (ord (line[0]) - ord ('a') + 5) % 7
                # FIXME: does key play any role in this?
		alteration = 0
                line = string.lstrip (line[1:])
		while line:
                        if len (line) > 1 and line[:2] == '//':
                                line = 0
                                break
			elif line[0] == '#':
				alteration = alteration + 1
			elif line[0] == '&':
				alteration = alteration - 1
			elif line[0] == '+':
                                oct = oct + 1 
			elif line[0] == '-':
                                oct = oct - 1
                        else:
                                skipping (_ ("%s") % line[0])
			line = string.lstrip (line[1:])
		return (oct, name, alteration)
			
	def parse_chord (self, line):
		line = string.lstrip (line)
		ch = Chord ()
		if not line:
			ch = self.current_voices[0].last_chord ()
		else:
			m = re.match ('^([0-9]+)([.]*)', line)
			if m:
				ch.basic_duration = string.atoi (m.group (1))
				line = line[len (m.group (1)):]
				if m.group (2):
					ch.basic_duration = len (m.group (2))
					line = line[len (m.group (1)):]
                        else:
                                ch.basic_duration = self.current_voices[0].last_chord ().basic_duration
                                
                        line = string.lstrip (line)
                        if len (line) > 1 and line[:2] == '//':
                                line = 0
                        #ugh
                        if not line:
                                duration = ch.basic_duration
                                ch = self.current_voices[0].last_chord ()
                                ch.basic_duration = duration
                                
			while line:
                                if len (line) > 1 and line[:2] == '//':
                                        line = 0
                                        break
                                elif line[:1] == 'mr':
					ch.multimeasure = 1
                                        line = line[1:]
                                elif line[:1] == 'ms':
					ch.multimeasure = 1
                                        line = line[1:]
				elif line[0] in 'rs':
                                        pass
				elif line[0] in 'abcdefg':
					pitch = self.parse_note (line)
                                        debug ('PITCH: ' + `pitch`)
					ch.pitches.append (pitch)
                                        line = 0
                                        break
				else:
					skipping (_ ("%s") % line[0])
                                line = string.lstrip (line[1:])
		map (lambda x, ch=ch: x.add_chord (ch), self.current_voices)

	def parse_voice (self, line):
		chords = string.split (line, ';')
		map (self.parse_chord, chords)

	def init_context_header (self, line):
		self.parse_function = self.parse_context_header
					
	def parse_context_header (self, line):
		debug ('header: ' + line)

	def init_context_footer (self, line):
		self.parse_function = self.parse_context_footer

	def parse_context_footer (self, line):
		debug ('footer: ' + line)

	def init_context_header2 (self, line):
		self.parse_function = self.parse_context_header2

	def parse_context_header2 (self, line):
		debug ('header2: ' + line)

	def init_context_footer2 (self, line):
		self.parse_function = self.parse_context_footer2

	def parse_context_footer2 (self, line):
		debug ('footer2: ' + line)

	def init_context_score (self, line):
		self.parse_function = self.parse_context_score

	def parse_context_score (self, line):
		debug ('score: ' + line)

	def init_context_staff (self, line):
		self.parse_function = self.parse_context_staff

	def parse_context_staff (self, line):
		debug ('staff: ' + line)

	def init_context_voice (self, line):
		self.parse_function = self.parse_context_voice

	def parse_context_voice (self, line):
		debug ('voice: ' + line)

	def init_context_grids (self, line):
		self.parse_function = self.parse_context_line

	def parse_context_grids (self, line):
		debug ('grids: ' + line)

	def init_context_music (self, line):
		self.parse_function = self.parse_context_music

	def parse_context_music (self, line):
		debug ('music: ' + line)
                line = string.lstrip (line)
                if line and line[0] in '0123456789':
                        line = string.lstrip (self.parse_compound_location (line))
                        self.parse_voice (line)
		else:
			skipping (_ ("%s") % line)
	
	def parse (self, file):
		# shortcut: set to official mup maximum (duh)
		# self.set_staffs (40)
		lines = open (file).readlines ()
		for line in lines:
                        debug ('LINE: ' + line)
			m = re.match ('^([a-z]+2?)', line)
			
			if m:
				word = m.group (1)
				if word in contexts:
					eval ('self.init_context_%s (line)' % word)
					continue
                                else:
                                        warning (_ ("no such context: %s") % word)
                                        skipping (line)
			else:
                                debug ('FUNC: ' + `self.parse_function`)
				self.parse_function (line)
				
		for c in self.staffs:
			c.calculate ()

	def dump (self):
		str = ''

		refs = ''
		for s in self.staffs:
			str = str +  s.dump ()
			refs = refs + '\n    \\' + s.idstring ()

		str = str + '''

\score {
  <%s
   >
}
''' % refs 
		return str

		
option_definitions = [
	('', 'd', 'debug', _ ("debug")),
	('', 'h', 'help', _ ("this help")),
	('FILE', 'o', 'output', _ ("write output to FILE")),
	('', 'V', 'verbose', _ ("verbose")),
	('', 'v', 'version', _ ("print version number")),
	('', 'w', 'warranty', _ ("show warranty and copyright")),
	]

debug_p = 0
def debug (s):
        if debug_p:
                progress ('DEBUG: ' + s)
def skipping (s):
        if debug_p:
                progress ('SKIPPING: ' + s)

(sh, long) = getopt_args (__main__.option_definitions)
try:
	(options, files) = getopt.getopt (sys.argv[1:], sh, long)
except:
	help ()
	sys.exit (2)


for opt in options:
	o = opt[0]
	a = opt[1]
        if 0:
                pass
	elif o== '--debug' or o == '-d':
                debug_p = 1
	elif o== '--help' or o == '-h':
		help ()
		sys.exit (0)
	elif o== '--verbose' or o == '-V':
                verbose_p = 1
	elif o == '--version' or o == '-v':
		identify ()
		sys.exit (0)
	elif o == '--output' or o == '-o':
		output = a
	else:
		print o
		raise getopt.error

# writes to stdout for help2man
# don't call 
# identify ()
# sys.stdout.flush ()

# handy emacs testing
if not files:
	files = ['template.mup']

for f in files:
	if f == '-':
		f = ''

	progress ( _("Processing %s..." % f))
	e = Parser (f)
	if not output:
		output = os.path.basename (re.sub ('(?i).mup$', '.ly', f))
		
	if output == f:
		output = os.path.basename (f + '.ly')
		
	progress (_ ("Writing %s...") % output)

	tag = '%% Lily was here -- automatically converted by %s from %s' % ( program_name, f)
	ly = tag + '\n' + e.dump ()

	o = open (output, 'w')
	o.write (ly)
	o.close ()
	print ly
	
