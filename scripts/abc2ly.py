#!@PYTHON@

# once upon a rainy monday afternoon.
#
#   ...
#
# (not finished.)
# ABC standard v1.6:  http://www.gre.ac.uk/~c.walshaw/abc2mtex/abc.txt
# 

program_name = 'abc-to-ly'
version = '@TOPLEVEL_VERSION@'
import __main__
import getopt
import sys
import re
import string
try:
	import mpz
except:
	sys.stderr.write ("This script needs Python 1.5.1\n")
	sys.exit (1)


header = {}
lyrics = []
voices = []
global_voice_stuff = []
default_len = 4
global_key = [0] * 7			# UGH
names = ["One", "Two", "Three"]
DIGITS='0123456789'
HSPACE=' \t'

def gcd (a, b):
	while  a % b:
		a,b = b, a % b
	return b
	
class Rational:
	def __init__ (self, n, d = 1):
		self.num = n
		self.den = d

	def simplify (self):
		g = gcd (self.num, self.den)
		self.num = self.num /  g
		self.den = self.den /g
		if self.den < 0:
			self.den = - self.den
			self.num = - self.num

	def __sub__ (self, other):
		pass
	

def dump_global ():
	print ("global = \\notes{")
	for i in global_voice_stuff:
		print (i);
	print ("}")


def dump_header (hdr):
	print '\\header {'
	for k in hdr.keys ():
		print '%s = "%s";\n'% (k,hdr[k])
 	print '}'

def dump_lyrics ():
	for i in range (len (lyrics)):
		print ("verse%s = \\lyrics {" % names [i])
		print (lyrics [i])
		print ("}")

def dump_voices ():
	for i in range (len (voices)):
		print ("voice%s = \\notes {" % names [i])
		print (voices [i])
		print ("}")
	
def dump_score ():
	print ("\\score{")
	print ("        \\notes<")
	print ("                \\global")
	for i in range (len (voices)):
		print ("        \\context Staff=%s \\voice%s" %
			(names [i], names [i]))
	for i in range (len (lyrics)):
		j = i
		if j >= len (voices):
			j = len (voices) - 1
		print ("        \\context Lyrics=%s \\rhythm \\voice%s \\verse%s" % 
			(names [i], names [j], names [i]))
	print ("    >")
	dump_header (header)
	#print "%%%s" % global_voice_stuff, 1
	print ("}")

def set_default_length (s):
	m =  re.search ('1/([0-9]+)', s)
	if m:
		__main__.default_len = string.atoi ( m.group (1))

def gulp_file(f):
	try:
		i = open(f)
		i.seek (0, 2)
		n = i.tell ()
		i.seek (0,0)
	except:
		sys.stderr.write ("can't open file: %s\n" % f)
		return ''
	s = i.read (n)
	if len (s) <= 0:
		sys.stderr.write ("gulped emty file: %s\n" % f)
	i.close ()
	return s


# pitch manipulation. Tuples are (name, alteration).
# 0 is (central) C. Alteration -1 is a flat, Alteration +1 is a sharp
# pitch in semitones. 
def semitone_pitch  (tup):
	p =0

	t = tup[0]
	p = p + 12 * (t / 7)
	t = t % 7
	
	if t > 2:
		p = p- 1
		
	p = p + t* 2 + tup[1]
	return p

def fifth_above_pitch (tup):
	(n, a)  = (tup[0] + 4, tup[1])

	difference = 7 - (semitone_pitch ((n,a)) - semitone_pitch (tup))
	a = a + difference
	
	return (n,a)

def sharp_keys ():
	p = (0,0)
	l = []
	k = 0
	while 1:
		l.append (p)
		(t,a) = fifth_above_pitch (p)
		if semitone_pitch((t,a)) % 12 == 0:
			break

		p = (t % 7, a)
	return l

def flat_keys ():
	p = (0,0)
	l = []
	k = 0
	while 1:
		l.append (p)
		(t,a) = quart_above_pitch (p)
		if semitone_pitch((t,a)) % 12 == 0:
			break

		p = (t % 7, a)
	return l

def quart_above_pitch (tup):
	(n, a)  = (tup[0] + 3, tup[1])

	difference = 5 - (semitone_pitch ((n,a)) - semitone_pitch (tup))
	a = a + difference
	
	return (n,a)


def compute_key (k):
	k = string.lower (k)
	intkey = (ord (k[0]) - ord('a') + 5) % 7
	intkeyacc =0
	k = k[1:]
	
	if k and k[0] == 'b':
		intkeyacc = -1
		k = k[1:]
	elif  k and k[0] == '#':
		intkeyacc = 1
		k = k[1:]

	keytup = (intkey, intkeyacc)
	
	sharp_key_seq = sharp_keys ()
	flat_key_seq = flat_keys ()

	accseq = None
	accsign = 0
	if keytup in sharp_key_seq:
		accsign = 1
		key_count = sharp_key_seq.index (keytup)
		accseq = map (lambda x: (4*x -1 ) % 7, range (1, key_count + 1))

	elif keytup in flat_key_seq:
		accsign = -1
		key_count = flat_key_seq.index (keytup)
		accseq = map (lambda x: (3*x + 3 ) % 7, range (1, key_count + 1))
	else:
		raise "Huh"
	
	key_table = [0] * 7
	for a in accseq:
		 key_table[a] = key_table[a] + accsign
		

	return key_table

tup_lookup = {
	'3' : '2/3',
	'4' : '4/3',
	'5' : '4/5',
	'6' : '4/6',
	}


def try_parse_tuplet_begin (str, state):
	if str and str[0] in DIGITS:
		dig = str[0]
		str = str[1:]
		state.parsing_tuplet = 1
		
		voices_append ("\\times %s {" % tup_lookup[dig])
	return str

def  try_parse_group_end (str, state):
	if str and str[0] in HSPACE:
		str = str[1:]
		if state.parsing_tuplet:
			state.parsing_tuplet = 0
			voices_append ("}")
	return str

def header_append (key, a):
	s = ''
	if header.has_key (key):
		s = header[key] + "\n"
	header [key] = s + a

def lyrics_append (a):
	i = len (lyrics) - 1
	if i < 0:
		i = 0
	if len (lyrics) <= i:
		lyrics.append ('')
	lyrics [i] = lyrics [i] + a + "\n"

def voices_append (a):
	i = len (voices) - 1
	if i < 0:
		i = 0
	if len (voices) <= i:
		voices.append ('')
	voices [i] = voices [i] + a + "\n"

def try_parse_header_line (ln):
	m = re.match ('^(.): *(.*)$', ln)

	if m:
		g =m.group (1)
		a = m.group (2)
		a = re.sub ('"', '\\"', a)
		if g == 'T':
			header['title'] =  a
		if g == 'M':
			if a == 'C':
				a = '4/4'
			global_voice_stuff.append ('\\time %s;' % a)
		if g == 'K':
			__main__.global_key  =compute_key (a)# ugh.

			global_voice_stuff.append ('\\key %s;' % a)
		if g == 'O': 
			header ['origin'] = a
		if g == 'X': 
			header ['crossRefNumber'] = a
		if g == 'A':
			header ['area'] = a
		if g == 'H':
			header_append ('history', a)
		if g == 'B':
			header ['book'] = a
		if g == 'S':
			header ['subtitle'] = a
		if g == 'L':
			set_default_length (ln)
		if g == 'W':
			if not len (a):
				lyrics.append ('')
			else:
				lyrics_append (a);
	return m

def pitch_to_mudela_name (name, acc):
	s = ''
	if acc < 0:
		s = 'es'
		acc = -acc
	elif acc > 0:
		s = 'is'

	if name > 4:
		name = name -7
	return chr (name  + ord('c'))  + s * acc

def octave_to_mudela_quotes (o):
	o = o + 2
	s =''
	if o < 0:
		o = -o
		s=','
	else:
		s ='\''

	return s * o

def parse_num (str):
	durstr = ''
	while str and str[0] in DIGITS:
		durstr = durstr + str[0]
		str = str[1:]

	n = None
	if durstr:
		n  =string.atoi (durstr) 
	return (str,n)


def duration_to_mudela_duration  (multiply_tup, defaultlen, dots):
	base = 1

	# (num /  den)  / defaultlen < 1/base
	while base * multiply_tup[0] < defaultlen * multiply_tup[1]:
		base = base * 2


	return '%d%s' % ( base, '.'* dots)

class Parser_state:
	def __init__ (self):
		self.next_dots = 0
		self.next_den = 1
		self.parsing_tuplet = 0


# WAT IS ABC EEN ONTZETTENDE PROGRAMMEERPOEP  !
def try_parse_note (str, parser_state):
	mud = ''

	slur_begin =0
	if not str:
		return str
	
	if  str[0] == '(':
		slur_begin = 1
		str = str[1:]

	acc = 0
	if str[0] in '^=_':
		c = str[0]
		str = str[1:]
		if c == '^':
			acc = 1
		if c == '=':
			acc = 0
		if c == '_':
			acc = -1

        octave = 0;
	if str[0] in "ABCDEFG":
		str = string.lower (str[0]) + str[1:]
		octave = -1


	notename = 0
	if str[0] in "abcdefg":
		notename = (ord(str[0]) - ord('a') + 5)%7
		str = str[1:]
	else:
		return str		# failed; not a note!

	while str[0] == ',':
		 octave = octave - 1
		 str = str[1:]
	while str[0] == '\'':
		 octave = octave + 1
		 str = str[1:]

	num = 0
	den = parser_state.next_den
	parser_state.next_den = 1

	(str, num) = parse_num (str)
	if not num:
		num = 1
	
	if str[0] == '/':
		while str[0] == '/':
			str= str[1:]
			d = 2
			if str[0] in DIGITS:
				(str, d) =parse_num (str)

			den = den * d

	current_dots = parser_state.next_dots
	parser_state.next_dots = 0
	while str[0] == '>':
		str = str [1:]
		current_dots = current_dots + 1;
		parser_state.next_den = parser_state.next_den * 2
	
	while str[0] == '<':
		str = str [1:]
		den = den * 2
		parser_state.next_dots = parser_state.next_dots + 1
	
		
	
	voices_append ("%s%s%s" %
		(pitch_to_mudela_name (notename, acc + global_key[notename]),
					octave_to_mudela_quotes (octave),
	 	 duration_to_mudela_duration ((num,den), default_len, current_dots)))
	slur_end =0
	if str[0] == ')':
		slur_begin = 1
		str = str[1:]


	return str

def junk_space (str):
	while str and str[0] in '\t\n ':
		str = str[1:]

	return str


def try_parse_guitar_chord (str):
	if str and str[0] == '"':
		str = str[1:]
		gc = ''
		while str and str[0] != '"':
			gc = gc + str[0]
			str = str[1:]
			
		if str:
			str = str[1:]

		sys.stderr.write ("warning: ignoring guitar chord: %s\n" % gc)
		
	return str

def try_parse_escape (str):
	if not str or str [0] != '\\':
		return str
	
	str = str[1:]
	if str and str[0] == 'K':
		key_table = compute_key ()

	return str

#
# |] thin-thick double bar line
# || thin-thin double bar line
# [| thick-thin double bar line
# :| left repeat
# |: right repeat
# :: left-right repeat
#

def try_parse_bar (str):
	if str and str[0] == '|':
		bs = ''
		str = str[1:]
		if str:
			if  str[0] == ']':
				bs = '|.'
			if str[0] == '|':
				bs = '||'
			if str[0] == '|:':
				sys.stderr.write ("warning: repeat kludge\n")
				bs = '|:'
		if bs:
			voices_append ('\\bar "%s";' % bs)
			str = str[1:]

	if str and str[:2] == '[|':
		sys.stderr.write ("warning: thick-thin bar kludge\n")
		voices_append ('\\bar "||";')
		str = str[2:]

	if str and str[:2] == ':|':
		sys.stderr.write ("warning: repeat kludge\n")
		voices_append ('\\bar ":|:";')
		str = str[2:]

	if str and str[:2] == '::':
		sys.stderr.write ("warning: repeat kludge\n")
		voices_append ('\\bar ":|:";')
		str = str[2:]

	return str
	

def try_parse_chord_delims (str):
	if str and str[0] == '[':
		str = str[1:]
		voices_append ('<')

	if str and str[0] == ']':
		str = str[1:]
		voices_append ('>')

	return str

# urg, hairy to compute grace note hack using \times{}
def try_parse_grace_delims (str):
	if str and str[0] == '{':
		str = str[1:]
		voices_append ('\\grace { ')

	if str and str[0] == '}':
		str = str[1:]
		voices_append ('}')

	return str

# Try nibbling characters off until the line doesn't change.
def try_parse_body_line (ln, state):
	prev_ln = ''
	while ln != prev_ln:
		prev_ln = ln
		ln = try_parse_chord_delims (ln)
		ln = try_parse_note  (ln, state)
		ln = try_parse_bar (ln)
		ln = try_parse_escape (ln)
		ln = try_parse_guitar_chord (ln)
		ln = try_parse_tuplet_begin (ln, state)
		ln = try_parse_group_end (ln, state)
		ln = try_parse_grace_delims (ln)
		ln = junk_space (ln)
		
	if ln:
		sys.stderr.write ("Huh?  Don't understand `%s'\n" % ln)
	


def parse_file (fn):
	f = open (fn)
	ls = f.readlines ()

	head = 1
	state = Parser_state ()
	for l in ls:
		if re.match ('^[\t ]*(%.*)?$', l):
			continue
		
		if head:
			m = try_parse_header_line (l)
			if not m:
				head = 0

		if not head:
			m = try_parse_body_line (l,state)


def identify():
	sys.stderr.write ("%s from LilyPond %s\n" % (program_name, version))

def help ():
	print r"""
This is a disfunctional ABC to mudela convertor.  It only gulps input, and
says huh when confused.  Go ahead and fix me.

Usage: abc-2-ly INPUTFILE

-h, --help   this help.
"""



identify()
(options, files) = getopt.getopt (sys.argv[1:], 'h', ['help'])

for opt in options:
	o = opt[0]
	a = opt[1]
	if o== '--help' or o == '-h':
		help ()
	else:
		print o
		raise getopt.error


for f in files:
	if f == '-':
		f = ''
	parse_file (f)

	dump_global ()
	dump_lyrics ()
	dump_voices ()
	dump_score ()
	
	
