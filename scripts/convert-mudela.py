#!@PYTHON@

# convert-mudela.py -- convertor for mudela versions
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1998 

# TODO
#   use -f and -t for -s output

# NEWS
# 0.2
#  - rewrite in python

program_name = 'convert-mudela'
version = '0.4'


import os
import sys
import __main__
import getopt
from string import *
import re

import time
mudela_version_re_str = '\\\\version *\"(.*)\"'
mudela_version_re = re.compile(mudela_version_re_str)

def program_id ():
	return '%s version %s' %(program_name,  version);

def identify ():
	sys.stderr.write (program_id () + '\n')

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


def str_to_tuple (s):
	return tuple (map (atoi, split (s,'.')))

def tup_to_str (t):
	return join (map (lambda x: '%s' % x, list (t)), '.')

def version_cmp (t1, t2):
	for x in [0,1,2]:
		if t1[x] - t2[x]:
			return t1[x] - t2[x]
	return 0
		

def guess_mudela_version(filename):
	s = gulp_file (filename)
	m = mudela_version_re.search (s)
	if m:
		return m.group(1)
	else:
		return ''

def help ():
	sys.stdout.write (
		("Usage: %s [OPTION]... [FILE]...\n" 
		+ "Try to convert to newer mudela-versions\n"
		+ "Options:\n"
		+ "  -h, --help             print this help\n"
		+ '  -e, --edit             in place edit\n'
		+ '  -f, --from=VERSION     start from version\n'
		+ '  -s, --show-rules       print out all rules.\n'
		+ '  -t, --to=VERSION       target version\n') % program_name)
	sys.exit (0)

class FatalConversionError:
	pass

conversions = []

def show_rules (file):
	for x in conversions:
		file.write  ('%s: %s\n' % (tup_to_str (x[0]), x[2]))

############################
		
if 1:					# need new a namespace
	def conv (lines):
		found =0
		for x in lines:
			if re.search ('\\\\octave', x):
				found = 1
				break
		if found:
			sys.stderr.write ('\nNot smart enough to convert \\octave')
			raise FatalConversionError()
		return lines
		

	conversions.append (
		((0,1,19), conv, 'deprecated \\octave; can\'t convert automatically'))


if 1:					# need new a namespace
	def conv (lines):
		newlines = []
		for x in lines:
			x = re.sub ('\\\\textstyle([^;]+);',
					 '\\\\property Lyrics . textstyle = \\1', x)
			x = re.sub ('\\\\key([^;]+);', '\\\\accidentals \\1;', x)
			newlines.append (x)
		return newlines
		

	conversions.append (
		((0,1,20), conv, 'deprecated \\textstyle, new \key syntax'))


if 1:					# need new a namespace
	def conv (lines):
		newlines = []
		for x in lines:
			x = re.sub ('\\\\musical_pitch',
					 '\\\\musicalpitch',x)
			x = re.sub ('\\\\meter',
					 '\\\\time',x)
			newlines.append (x)
		return newlines
		

	conversions.append (
		((0,1,21), conv, '\\musical_pitch -> \\musicalpitch, '+
		 '\\meter -> \\time'))

if 1:					# need new a namespace
	def conv (lines):
		return lines

	conversions.append (
		((1,0,0), conv, '0.1.21 -> 1.0.0 '))


if 1:					# need new a namespace
	def conv (lines):
		newlines = []
		for x in lines:
			x = re.sub ('\\\\accidentals',
				    '\\\\keysignature',x)
			x = re.sub ('specialaccidentals *= *1',
					 'keyoctaviation = 0',x)
			x = re.sub ('specialaccidentals *= *0',
					 'keyoctaviation = 1',x)
			newlines.append (x)
		return newlines
		

	conversions.append (
		((1,0,1), conv, '\\accidentals -> \\keysignature, ' +
		 'specialaccidentals -> keyoctaviation'))

if 1:
	def conv(lines):
		found = 0
		for x in lines:
			if re.search ('\\\\header', x):
				found = 1
				break
		if found:
			sys.stderr.write ('\nNot smart enough to convert to new \\header format')
		return lines
	
	conversions.append ((1,0,2), conv, '\\header { key = concat + with + operator }')

if 1:
	def conv(lines):
		newlines =[]
		for x in lines:
			x =  re.sub ('\\\\melodic', '\\\\notes',x)
			newlines.append (x)
		return newlines
	
	conversions.append ((1,0,3), conv, '\\melodic -> \\notes')

if 1:
	def conv(lines):
		newlines =[]
		for x in lines:
			x =  re.sub ('default_paper *=', '',x)
			x =  re.sub ('default_midi *=', '',x)			
			newlines.append (x)
		return newlines
	
	conversions.append ((1,0,4), conv, 'default_{paper,midi}')

if 1:
	def conv(lines):
		newlines =[]
		for x in lines:
			x =  re.sub ('ChoireStaff', 'ChoirStaff',x)
			x =  re.sub ('\\output', 'output = ',x)
			newlines.append (x)
		return newlines
	
	conversions.append ((1,0,5), conv, 'ChoireStaff -> ChoirStaff')

if 1:
	def conv(lines):
		newlines =[]
		found = None
		for x in lines:
			found = re.search ('[a-zA-Z]+ = *\\translator',x)
			newlines.append (x)
			if found: break
		if found:
			sys.stderr.write ('\nNot smart enough to \\translator syntax')
			raise FatalConversionError()
		return newlines
	
	conversions.append ((1,0,6), conv, 'foo = \\translator {\\type .. } ->\\translator {\\type ..; foo; }')


if 1:
	def conv(lines):
		newlines =[]
		for x in lines:
			x =  re.sub ('\\\\lyric', '\\\\lyrics',x)
			newlines.append (x)
		return newlines
	
	conversions.append ((1,0,7), conv, '\\lyric -> \\lyrics')


############################
	

def get_conversions (from_version, to_version):
	def version_b (v, f = from_version, t = to_version):
		return version_cmp (v[0], f) > 0 and version_cmp (v[0], t) <= 0
	return filter (version_b, conversions)


def latest_version ():
	return conversions[-1][0]

def do_conversion (infile, from_version, outfile, to_version):
	
	conv_list = get_conversions (from_version, to_version)

	sys.stderr.write ('Applying conversions: ')
	lines = infile.readlines();
	last_conversion = ()
	try:
		for x in conv_list:
			sys.stderr.write (tup_to_str (x[0])  + ', ')
			lines = x[1] (lines)
			last_conversion = x[0]
			

	except FatalConversionError:
		sys.stderr.write ('Error while converting; I won\'t convert any further')

	for x in lines:
		if last_conversion:
			x = re.sub (mudela_version_re_str, '\\\\version \"%s\"' % tup_to_str (last_conversion), x)
		outfile.write(x)

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
		guess = guess_mudela_version (infile_name)
		if not guess:
			raise UnknownVersion()
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

	
	do_conversion (infile, from_version, outfile, to_version)

	if infile_name:
		infile.close ()

	if outfile_name:
		outfile.close ()

	if __main__.edit:
		try:
			os.remove(infile_name + '~')
		except:
			pass
		os.rename (infile_name, infile_name + '~')
		os.rename (infile_name + '.NEW', infile_name)

	sys.stderr.write ('\n')
	sys.stderr.flush ()

edit = 0
to_version = ()
from_version = ()
outfile_name = ''

identify ()
(options, files) = getopt.getopt (
	sys.argv[1:], 'f:t:seh', ['show-rules', 'help', 'edit', 'from', 'to'])

for opt in options:
	o = opt[0]
	a = opt[1]
	if o== '--help' or o == '-h':
		help ()
	elif o== '--from' or o=='-f':
		from_version = str_to_tuple (a)
	elif o== '--to' or o=='-t':
		to_version = str_to_tuple (a)
	elif o== '--edit' or o == '-e':
		edit = 1
	elif o== '--show-rules' or o == '-s':
		show_rules (sys.stdout)
		sys.exit(0)
	else:
		print o
		raise getopt.error


for f in files:
	if f == '-':
		f = ''
	try:
		do_one_file (f)
	except UnknownVersion:
		pass
