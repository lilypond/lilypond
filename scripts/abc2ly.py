#!@PYTHON@

# once upon a rainy monday afternoon.
#
#   ...
#
# (not finished.)
# ABC standard v1.6:  http://www.gre.ac.uk/~c.walshaw/abc2mtex/abc.txt
# 


program_name = 'abc2ly'
version = '@TOPLEVEL_VERSION@'
if version == '@' + 'TOPLEVEL_VERSION' + '@':
	version = '1.2.0'
import __main__
import getopt
import sys
import re
import string
import os
try:
	import mpz
except:
	sys.stderr.write ("This script needs Python 1.5.1\n")
	sys.exit (1)


voice_idx_dict = {}


header = {}
lyrics = []
voices = []
current_voice_idx = -1
current_lyric_idx = -1

def select_voice (name):
	if not voice_idx_dict.has_key (name):
		voices.append ('')		
		voice_idx_dict[name] = len (voices) -1
	__main__.current_voice_idx =  voice_idx_dict[name]
	
#	assert 0
# current_voice_idx >= 0

default_len = 8
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
	


def dump_header (outf,hdr):
	outf.write ('\\header {')
	ks = hdr.keys ()
	ks.sort ()
	for k in ks:
		outf.write ('\n%s = "%s";\n'% (k,hdr[k]))
 	outf.write ('}')

def dump_lyrics (outf):
	for i in range (len (lyrics)):
		outf.write ("\nverse%s = \\lyrics {" % names [i])
		outf.write ("\n" + lyrics [i])
		outf.write ("\n}")

def dump_voices (outf):
	ks = voice_idx_dict.keys()
	ks.sort ()
	for k in ks:
		outf.write ("\nvoice%s = \\notes {" % k)
		outf.write ("\n" + voices [voice_idx_dict[k]])
		outf.write ("\n}")
	
def dump_score (outf):
	outf.write (r"""\score{
        \notes <
""")

	ks  = voice_idx_dict.keys ();
	ks.sort ()
	for k in  ks:
		outf.write ("\n        \\context Staff=\"%s\" \\$voice%s " % (k,k))# ugh
	for i in range (len (lyrics)):
		j = i
		if j >= len (voices):
			j = len (voices) - 1
		outf.write ("\n        \\context Lyrics=\"%s\" \\addlyrics \\$voice%s \\$verse%s " % 
			(names [i], names [j], names [i]))
	outf.write ("\n    >")
	dump_header (outf ,header)
	outf.write (r"""
\paper {}
\midi {}
}""")

def set_default_length (s):
	m =  re.search ('1/([0-9]+)', s)
	if m:
		__main__.default_len = string.atoi ( m.group (1))

def set_default_len_from_time_sig (s):
	m =  re.search ('([0-9]+)/([0-9]+)', s)
	if m:
		n = string.atoi (m.group (1))
		d = string.atoi (m.group (2))
		if (n * 1.0 )/(d * 1.0) <  0.75:
			default_len =  16
		else:
			default_len = 8

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
	'2' : '3/2',
	'3' : '2/3',
	'4' : '4/3',
	'5' : '4/5',
	'6' : '4/6',
	'7' : '6/7',
	'9' : '8/9',
	}


def try_parse_tuplet_begin (str, state):
	if re.match ('\([0-9]', str):
		dig = str[1]
		str = str[2:]
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

def stuff_append (stuff, idx, a):
	if not stuff:
		stuff.append ('')

	v = stuff[idx]

	#wordwrap
	linelen = len (v) - string.rfind(v, '\n')
	if linelen + len (a) > 80:
		v = v + '\n'
	v = v + a + ' '
	stuff [idx] = v



def voices_append(a):
	if current_voice_idx < 0:
		select_voice ('default')

	stuff_append (voices, current_voice_idx, a)

def lyrics_append(a):
	stuff_append (lyrics, current_lyric_idx, a)


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
#			global_voice_stuff.append ('\\time %s;' % a)
			set_default_len_from_time_sig (a)
			voices_append ('\\time %s;' % a)
		if g == 'K':
			__main__.global_key  =compute_key (a)# ugh.
			voices_append ('\\key %s;' % a)

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
		if g == 'C':
			header ['composer'] = a
		if g == 'S':
			header ['subtitle'] = a
		if g == 'L':
			set_default_length (ln)
		if g == 'V':
			a = re.sub (' .*$', '', a)
			select_voice (a)
		if g == 'W':
			if not len (a):
				lyrics.append ('')
			else:
				lyrics_append (a);

		return ''
	return ln

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
	while base * multiply_tup[0] < multiply_tup[1]:
		base = base * 2


	return '%d%s' % ( base, '.'* dots)

class Parser_state:
	def __init__ (self):
		self.next_articulation = ''
		self.next_dots = 0
		self.next_den = 1
		self.parsing_tuplet = 0



# return (str, num,den,dots) 
def parse_duration (str, parser_state):
	num = 0
	den = parser_state.next_den
	parser_state.next_den = 1

	(str, num) = parse_num (str)
	if not num:
		num = 1
	
	if str[0] == '/':
		while str[:1] == '/':
			str= str[1:]
			d = 2
			if str[0] in DIGITS:
				(str, d) =parse_num (str)

			den = den * d

	den = den * default_len
	
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



	try_dots = [3, 2, 1]
	for d in try_dots:
		f = 1 << d
		multiplier = (2*f-1)
		if num % multiplier == 0 and den % f == 0:
			num = num / multiplier
			den = den / f
			current_dots = current_dots + d
		
	return (str, num,den,current_dots)


def try_parse_rest (str, parser_state):
	if not str or str[0] <> 'z':
		return str

	str = str[1:]

	(str, num,den,d) = parse_duration (str, parser_state)
	voices_append ('r%s' % duration_to_mudela_duration ((num,den), default_len, d))

	return str

def try_parse_articulation (str, state):
	
	if str[:1] =='.':
		state.next_articulation = state.next_articulation + '-.'
		str = str[1:]

	if str[:1] =='~':
		state.next_articulation = state.next_articulation + '-\\trill'
		str = str[1:]
		
	# s7m2 input doesnt care about spaces
	if re.match('[ \t]*\(', str):
		str = string.lstrip (str)

	slur_begin =0
	while str[:1] =='(' and str[1] not in DIGITS:
		slur_begin = slur_begin + 1
		state.next_articulation = state.next_articulation + '('
		str = str[1:]

	return str
		
# WAT IS ABC EEN ONTZETTENDE PROGRAMMEERPOEP  !
def try_parse_note (str, parser_state):
	mud = ''

	slur_begin =0
	if not str:
		return str

	articulation =''
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

	(str, num,den,current_dots) = parse_duration (str, parser_state)


	if re.match('[ \t]*\)', str):
		str = string.lstrip (str)
	
	slur_end =0
	while str[:1] ==')':
		slur_end = slur_end + 1
		str = str[1:]

	
	if slur_end:
		voices_append ('%s' % ')' *slur_end )
	voices_append ("%s%s%s" %
		(pitch_to_mudela_name (notename, acc + global_key[notename]),
					octave_to_mudela_quotes (octave),
	 	 duration_to_mudela_duration ((num,den), default_len, current_dots)))
	if parser_state.next_articulation:
		articulation = articulation + parser_state.next_articulation
		parser_state.next_articulation = ''

	voices_append (articulation)
	if slur_begin:
		voices_append ('%s' % '(' * slur_begin )


	return str

def junk_space (str):
	while str and str[0] in '\t\n ':
		str = str[1:]

	return str


def try_parse_guitar_chord (str, state):
	if str[:1] =='"':
		str = str[1:]
		gc = ''
		while str and str[0] != '"':
			gc = gc + str[0]
			str = str[1:]
			
		if str:
			str = str[1:]

		state.next_articulation = "-\"%s\"" % gc
	return str

def try_parse_escape (str):
	if not str or str [0] != '\\':
		return str
	
	str = str[1:]
	if str[:1] =='K':
		key_table = compute_key ()

	return str

#
# |] thin-thick double bar line
# || thin-thin double bar line
# [| thick-thin double bar line
# :| left repeat
# |: right repeat
# :: left-right repeat
# |1 volta 1
# |2 volta 2
bar_dict = {
'|]' : '|.',
'||' : '||',
'[|' : '||',
':|' : ':|',
'|:' : '|:',
'::' : '::',
'|1' : '|',
'|2' : '|',
':|2' : ':|'
}


warn_about = ['|:', '::', ':|', '|1', ':|2', '|2']

def try_parse_bar (str,state):
	bs = None

	# first try the longer one
	for trylen in [3,2]:
		if str[:trylen] and bar_dict.has_key (str[:trylen]):
			s = str[:trylen]
			bs = "\\bar \"%s\";" % bar_dict[s]
			if s in warn_about:
				sys.stderr.write('Warning kludging for barline `%s\'\n' % s)
			str = str[trylen:]
			break

	if str[:1] == '|':
		bs = '|\n'
		str = str[1:]
	
	if bs <> None:
		if state.parsing_tuplet:
			state.parsing_tuplet =0
			voices_append ('} ')
		
		voices_append (bs)

	return str

def try_parse_tie (str):
	if str[:1] =='-':
		str = str[1:]
		voices_append (' ~ ')
	return str

def try_parse_chord_delims (str):
	if str[:1] =='[':
		str = str[1:]
		voices_append ('<')

	ch = ''
	if str[:1] ==']':
		str = str[1:]
		ch = '>'

	end = 0
	while str[:1] ==')':
		end = end + 1
		str = str[1:]

	
	voices_append ("\\spanrequest \\stop \"slur\"" * end);
	voices_append (ch)
	return str

def try_parse_grace_delims (str):
	if str[:1] =='{':
		str = str[1:]
		voices_append ('\\grace { ')

	if str[:1] =='}':
		str = str[1:]
		voices_append ('}')

	return str


happy_count = 100
def parse_file (fn):
	f = open (fn)
	ls = f.readlines ()

	state = Parser_state ()
	lineno = 0
	sys.stderr.write ("Line ... ")
	sys.stderr.flush ()
	
	for ln in ls:
		lineno = lineno + 1

		if not (lineno % happy_count):
			sys.stderr.write ('[%d]'% lineno)
			sys.stderr.flush ()
		if re.match ('^[\t ]*(%.*)?$', ln):
			continue
		m = re.match  ('^(.*?)%(.*)$',ln)
		if m:
			voices_append ('%% %s\n' % m.group(2))
			ln = m.group (1)

		orig_ln = ln
		
		ln = try_parse_header_line (ln)

		# Try nibbling characters off until the line doesn't change.
		prev_ln = ''
		while ln != prev_ln:
			prev_ln = ln
			ln = try_parse_chord_delims (ln)
			ln = try_parse_rest (ln, state)
			ln = try_parse_articulation (ln,state)
			ln = try_parse_note  (ln, state)
			ln = try_parse_bar (ln, state)
			ln = try_parse_tie (ln)
			ln = try_parse_escape (ln)
			ln = try_parse_guitar_chord (ln, state)
			ln = try_parse_tuplet_begin (ln, state)
			ln = try_parse_group_end (ln, state)
			ln = try_parse_grace_delims (ln)
			ln = junk_space (ln)

		if ln:
			msg = "%s: %d: Huh?  Don't understand\n" % (fn, lineno)
			sys.stderr.write (msg)
			left = orig_ln[0:-len (ln)]
			sys.stderr.write (left + '\n')
			sys.stderr.write (' ' *  len (left) + ln + '\n')	


def identify():
	sys.stderr.write ("%s from LilyPond %s\n" % (program_name, version))

def help ():
	print r"""
Convert ABC to Mudela.

Usage: abc2ly [OPTION]... ABC-FILE

Options:
  -h, --help          this help
  -o, --output=FILE   set output filename to FILE
"""


identify()
(options, files) = getopt.getopt (sys.argv[1:], 'o:h', ['help', 'output='])
out_filename = ''

for opt in options:
	o = opt[0]
	a = opt[1]
	if o== '--help' or o == '-h':
		help ()
	if o == '--output' or o == '-o':
		out_filename = a
	else:
		print o
		raise getopt.error


header['tagline'] = 'Lily was here %s -- automatically converted from ABC' % version
for f in files:
	if f == '-':
		f = ''

	sys.stderr.write ('Parsing... [%s]\n' % f)
	parse_file (f)

	if not out_filename:
		out_filename = os.path.basename (os.path.splitext (f)[0]) + ".ly"
	sys.stderr.write ('Ly output to: %s...' % out_filename)
	outf = open (out_filename, 'w')

#	dump_global (outf)
	dump_lyrics (outf)
	dump_voices (outf)
	dump_score (outf)
	sys.stderr.write ('\n')
	
