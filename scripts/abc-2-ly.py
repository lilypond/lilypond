#!@PYTHON@

# once upon a rainy monday afternoon.
#
#   ...
#
# (not finished.)
# 

name = 'abc-to-ly'
version = '0.1'

import getopt
import sys
import re
import string
header = {}
global_voice_stuff = []
default_len = 4


def dump_header (hdr):
	print '\\header {'
	for k in hdr.keys ():
		print '%s = "%s";\n'% (k,hdr[k])
 	print '};'

def set_default_length (s):
	m =  re.match ('1/(.*)$', s)
	if m:
		default_len = string.atoi ( m.group (1))

def parse_timesig (s):
	m =  re.match ('^M: *(.*)$', s)
	if m:
		print '\meter %s; ' % m.group (1)

def parse_key (s):
	m =  re.match ('^K: *(.*)$', s)
	if m:
		print '\key %s; ' % m.group (1)
	
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


# WAT IS ABC EEN ONTZETTENDE PROGRAMMEERPOEP  !

def try_parse_note (str):
	mud = ''

	slur_begin =0
	if str[0] == '(':
		slur_begin = 1
		str = str[1:]

	acc = None
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
		notename = ord(str[0]) - ord('a')
		str = str[1:]
	else:
		return str		# failed; not a note!

	while str[0] == ',':
		 octave = octave - 1
		 str = str[1:]
	while str[0] == '\'':
		 octave = octave + 1
		 str = str[1:]
	divide =0
	if str[0] == '/':
		divide =1
		str = str[1:]
	durstr = ''
	while str[0] in "1234567890":
		durstr = durstr + str[0]
		str = str[1:]

	duration_mult = 1
	if durstr:
		duration_mult = string.atoi (durstr)

	
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
	print '%s %s' % (name, version)

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
	
