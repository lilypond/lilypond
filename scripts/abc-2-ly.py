#!@PYTHON@

# once upon a rainy monday afternoon.
#
#   ...
#
# (not finished.)
# 

program_name = 'abc-to-ly'
version = '0.1'
import __main__
import getopt
import sys
import re
import string


header = {}
global_voice_stuff = []
default_len = 4
global_key = [0] * 7			# UGH



def dump_header (hdr):
	print '\\header {'
	for k in hdr.keys ():
		print '%s = "%s";\n'% (k,hdr[k])
 	print '};'

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
		print 'can\'t open file: ' + f + '\n'
		return ''
	s = i.read (n)
	if len (s) <= 0:
		print 'gulped empty file: ' + f + '\n'
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

def try_parse_header_line (ln):
	m = re.match ('^(.): *(.*)$', ln)

	if m:
		g =m.group (1)
		a = m.group (2)
		if g == 'T':
			header['title'] =  a
		if g == 'M':
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
			header ['history'] = a
		if g == 'B':
			header ['book'] = a
		if g == 'S':
			header ['subtitle'] = a
		if g == 'L':
			set_default_length (ln)
	

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
	s =''
	if o < 0:
		o = -o
		s=','
	else:
		s ='\''

	return s * o

def parse_num (str):
	durstr = ''
	while str[0] in "1234567890":
		durstr = durstr + str[0]
		str = str[1:]

	n = None
	if durstr:
		n  =string.atoi (durstr) 
	return (str,n)


def duration_to_mudela_duration  (multiply_tup, defaultlen):
	base = 1

# WAT IS ABC EEN ONTZETTENDE PROGRAMMEERPOEP  !
def try_parse_note (str):
	mud = ''

	slur_begin =0
	if str[0] == '(':
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
	den = 0

	(str, num) = parse_num (str)
	if not num:
		num = 1
	
	if str[0] == '/':
		divide =1
		str = str[1:]
		(str, den) =parse_num (str)

	if not den: den = 1
		
	print duration_to_mudela_duration ((num,den), default_len)
	print '%s%s%d' %  (pitch_to_mudela_name(notename, acc + global_key[notename]) , octave_to_mudela_quotes (octave), duration_mult)

	slur_end =0
	if str[0] == ')':
		slur_begin = 1
		str = str[1:]


	return str

def junk_space (str):
	while str and str[0] in '\t\n ':
		str = str[1:]

	return str

def try_parse_bar (str):
	if str[0] == '|':
		str = str[1:]
	return str
	
def try_parse_body_line (ln):
	prev_ln = ''
	while ln and  ln != prev_ln:
		prev_ln = ln
		ln = try_parse_note  (ln)
		ln = try_parse_bar (ln)
		ln = junk_space (ln)
	if ln:
		print 'Huh %s' % ln
		

def parse_file (fn):
	f = open (fn)
	ls = f.readlines ()

	head = 1
	for l in ls:
		if re.match ('^[\t ]*(%.*)?$', l):
			continue
		
		if head:
			m = try_parse_header_line (l)
			if not m:
				head = 0

		if not head:
			m = try_parse_body_line (l)


def identify():
	print '%s %s' % (program_name, version)

def help ():
	print r"""
This is a disfunctional ABC to mudela convertor.  It only gulps input, and
says huh when confused.  Does not do chords.  Go ahead and fix me.

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
	dump_header (header)
	print global_voice_stuff, 1
	
