#!/usr/bin/python
#!@PYTHON@

program_name = 'musa.py'
version = '@TOPLEVEL_VERSION@'
if version == '@' + 'TOPLEVEL_VERSION' + '@':
	version = '(unknown version)'		# uGUHGUHGHGUGH

import __main__
import getopt
import sys
import re
import string
import os

#names = ["One", "Two", "Three"]
DIGITS='0123456789'

def dump_score (outf):
	outf.write (r"""\score{
        \notes <
""")

def set_default_len_from_time_sig (s):
	m =  re.search ('([0-9]+)/([0-9]+)', s)
	if m:
		n = string.atoi (m.group (1))
		d = string.atoi (m.group (2))
		if (n * 1.0 )/(d * 1.0) <  0.75:
			__main__.default_len =  16
		else:
			__main__.default_len = 8

def gulp_file (f):
	try:
		i = open (f)
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

def identify():
	sys.stderr.write ("%s from LilyPond %s\n" % (program_name, version))

def help ():
	print r"""
Musa.

Usage: musa [OPTION]...

Options:
  -h, --help          this help
  -o, --output=FILE   set output filename to FILE
  -v, --version       version information
"""

def print_version ():
	print r"""musa (GNU lilypond) %s""" % version


(options, files) = getopt.getopt (sys.argv[1:], 'vo:h', ['help','version', 'output='])
out_filename = ''

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

identify ()

#header['tagline'] = 'Lily was here %s -- automatically converted from ABC' % version

#if not out_filename:
	#out_filename = os.path.basename (os.path.splitext (f)[0]) + ".ly"
#sys.stderr.write ('Ly output to: %s...' % out_filename)
#outf = open (out_filename, 'w')
#sys.stderr.write ('\n')

outf = sys.stdout

# display width
width = 65

# font database
fonts = {}

# cursor
x = 0
y = 0

# current font
font = ""

def print_font (name):
	global fonts
	font = fonts[name]
	for k in font.keys ():
		c = font[k]
		print ("Character: %s" % k)
		for i in range (len (c)):
			print (c[i])

def put_char (x, y, c):
	global line, width
	height = len (line[0])
	y = -y
	if x >= 0 and x < width - 2 and y >= 0 and y < height:
		try:
			line[y] = line[y][:x] + c + line[y][x+1:]
		except:
			print ("%d, %d: %c" % (x, y, c))
	else:
		print ("%d, %d: %c" % (x, y, c))

def put_string (x, y, s):
	global line, width
	height = len (line[0])
	y = -y
	if x >= 0 and x < width and y >= 0 and y < height:
		try:
			line[y] = line[y][:x] + s + line[y][x+len (s):]
		except:
			print ("%d, %d: %s" % (x, y, s))
	else:
		print ("%d, %d: %s" % (x, y, s))

def header (creator, generate):
	print (creator, generate)

def header_end ():
	return

def load_font (name, mag):
	global fonts
	font_str = gulp_file (name + ".af");
	i = 0
	font = {}
	for c in string.split (font_str, '\f')[1:]:
		id = 0
		code = 0
		char = []
		for line in string.split (c, '\n')[:-1]:
			if not id:
				id = line
				#code = string.atoi (id[string.index (id, 
				#	'C')+2:string.index (id, ';')])
				code = id[string.index (id, 
					'C')+2:string.index (id, ';')]
				code = string.strip (code)
				bbox = string.split (string.strip (id[string.rindex (id, 'B')+1: string.rindex (id, ';')]), ' ')
				char.append (bbox)
			else:
				char.append (line)
		font[code] = char
	fonts[name] = font
	#print_font (name)

def start_line (height):
	global line
	line = []
	# urg
	for i in range (height+2):
		line.append (" " * width)

def move_to (new_x, new_y):
	global x, y
	x = new_x
	y = new_y

def move_relative (dx, dy):
	global x, y
	x = x + dx
	y = y + dy

def hline (length):
	global x, y
	for i in range (length):
		put_char (x+i, y, '-')
	return

def select_font (name):
	global font
	font = name

def char (i):
	global x, y, width, fonts, font
	height = len (line[0])
	#y = -y
	#if x >= 0 and x < width and y >= 0 and y < height:
	c = fonts[font][`i`]
	bbox = c[0]
	#print ("Bbox: %s " % `bbox`)
	c = c[1:]
	for i in range (len (c)):
		put_string (x-string.atoi (bbox[1]), y-i+ string.atoi (bbox[3]), c[i])

def text (s):
	global x, y
	put_string (x, y, s)

def vline (length):
	global x, y
	for i in range (length):
		put_char (x, y+i, '|')

def stop_line ():
	global line
	for i in range (len (line)):
		print (line[i])
	print ("=== ===")

def end_output ():
	return

