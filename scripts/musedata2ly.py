#!@PYTHON@

# musedata = musedata.stanford.edu

import re
import sys
import string

f = open (sys.argv[1])
lines =f.readlines()

def chomp (x):
	return re.sub ('[\r\n \t]+$','', x)

lines = map (chomp, lines)

default_header_dict = {
	'tagline' :'automatically converted from Musedata',
	'copyright' : 'all rights reserved -- free for noncommercial use'
	}

# Jezus, wat een ranzig formaat. (2am)
def parse_header (lines):
	d = default_header_dict
	enter = string.split (lines[3], ' ')
	d['enteredby']  = string.join (enter[1:])
	d['enteredon'] = enter[0]
	d['opus'] = lines[4]
	d['source'] = lines[5]
	d['title'] = lines[6]
	d['subtitle'] = lines[7]
	d['instrument']= lines[8]
	d['musedatamisc'] =lines[9]
	d['musedatagroups'] =lines[10]
	d['musedatagroupnumber']=lines[11]

	return d

clef_dict = {
04: 'treble',
13 : 'alto',
22: 'bass',

}

def get_clef(s):
	return '\\clef "%s";\n' % clef_dict [string.atoi (s)]

def get_mudela_notename (p, ac):
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

def get_key (s):
	i = string.atoi (s)
	return ''

def get_timesig (s):
	return '\\time %s;\n' % s


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

attr_dict = {
	'C' : get_clef,
	'K' : get_key ,
	'T' : get_timesig,
	'Q' : get_divisions_per_quarter,
	'D' : get_directive,
	'X' : get_transposing,
	'I': get_num_instruments,
	}

def parse_musical_attributes (l):
	s = ''
	l = l[1:]
	atts = re.split('[ \t]+', l)
	for a in atts:
		if not a:
			continue
		m = re.search ('(.):(.*)', a)
		if m == None:
			print 'Huh, unknown attr `%s\'' % a
			continue

		s = s + attr_dict[m.group(1)](m.group (2))
	return s


def get_mudela_pitch (n, a, o):
	c = '\''
	if o < 1:
		c = ','
		o = 1 - o

	return get_mudela_notename (n,a) +  '%s' % c * o

def dump_header (h, out):
	out.write ('\\header {\n')
	for tup in h.items ():
		out.write ('\t%s = \"%s\";\n' % tup)
	out.write ('}\n')
		
header_dict = parse_header (lines[0:12])
dump_header (header_dict, sys.stdout)


lines  = lines [12:]


def parse_line_comment (l):
	return re.sub ('@' , '%' , l)

def parse_note_line (l):
	pitch = ((ord (l[0]) -ord('A')) + 5) % 7
	acc = 0
	l= l[1:]
	while l[0] == 'f':
		l= l[1:]
		acc = acc - 1
	while l[0] == '#':
		l= l[1:]
		acc = acc + 1
	while l[0] in ' \t':
		l= l[1:]
		
	oct = 0
	if l[0] in '0123456789':
		oct = string.atoi (l[0]) - 4
		l= l[1:]

	while l[0] in ' \t':
		l= l[1:]

	
	print get_mudela_pitch (pitch,acc,oct), parse_duration(l[:2])
	l = l[2:]
	
	

	
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
	
comment_switch = 0
for l in lines:
	if l[0] == '&':
		comment_switch = not comment_switch
		if comment_switch:
			l= l[1:]
			print '%{'
		else:
			print '%}'
			
	if comment_switch:
		print l
		continue

	if 0:
		pass
	elif l[0] == '$':
		print parse_musical_attributes (l)
	elif l[0] == '@':
		parse_line_comment (l)

	elif l[0] in 'ABCDEFG':
		parse_note_line (l)
