#!/usr/bin/python
#!@PYTHON@

# mup-to-ly.py -- 
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>

# TODO: regex -> re.

name = 'mup-to-ly'
version = '0.1'

import os
import sys

import getopt
from string import *
import regex
import regsub
import time

def program_id ():
    return name + ' version ' + version;

def identify ():
    sys.stdout.write (program_id () + '\n')

def help ():
    sys.stdout.write ("Usage: %s [options] [files]\n"
                      "Convert mup to ly\n\n"
                      + "Options:\n"
                      + "  -h, --help             print this help\n"
                      % (program_name)
		      )
    sys.exit (0)

identify ()
(options, files) = getopt.getopt (
    sys.argv[1:], 'hp:', ['help', 'package'])
for opt in options:
    o = opt[0]
    a = opt[1]
    if o== '--help' or o == '-h':
	help ()
    elif o == '-p' or o == '--package':
	topdir = a
    else:
	print o
	raise getopt.error

def gulp_file (f):
	sys.stderr.write ('[%s' % f)
	try:
		i = open (f)
		i.seek (0, 2)
		n = i.tell ()
		i.seek (0,0)
	except:
		sys.stderr.write ('can\'t open file %s\n ' % f)
		return ''
	s = i.read (n)
	sys.stderr.write (']')
	if len (s) <= 0:
		sys.stderr.write ('gulped empty file: %s\n'% f)
	return s

def line_to_ly (s):
	notes = ""
	o = 0
	i = regex.search (";", s)
	last_name = "c"
	last_duration = "4"
	while i >= 0:
		note = s[o:o+i]
		o = o + i
		i = regex.search (";", s[o+1:])
		if i >= 0 :
			o = o + 1
		name = regsub.gsub ("[0-9<>\.&]*", "", note)
		duration = regsub.gsub ("[a-z+<>#+&\-]*", "", note)
		duration = regsub.gsub (" ", "", duration)
		if name:
			last_name = name
		else:
			name = last_name
		if duration:
			last_duration = duration
		else:
			duration = last_duration
		name = regsub.sub ("#", "is", name)
		name = regsub.sub ("+", "'", name)
		name = regsub.sub ("-", ",", name)
		name = regsub.sub ("ms", "s1", name)
		notes = notes + " %s%s" % (name, duration)
	return notes

def get_voice (staff, s, staffs):
	voice = len (staffs[staff-1]) + 1
	tag = "^%d [0-9-]*%d[0-9-]*:" % (staff, voice)
	notes = ""
	o = 0
	i = regex.search (tag, s)
	while i >= 0:
		o = o + i
		n = regex.search ("$", s[o:])
		line = s[o:o+n]
		line = regsub.sub (tag, "", line)
		line = line_to_ly (line)
		notes = notes + line
		i = regex.search (tag, s[o+n:])
		if i >= 0:
			i = i + n
	if notes != "":
		sys.stderr.write ('%d ' % voice)
		staffs[staff-1].append (notes)
	return notes != ""

def get_staff (s, staffs):
	staff=len (staffs)
	i=1
	sys.stderr.write ('Staff %d ( ' % staff)
	while i: 
		i = get_voice (staff, s, staffs)
		if not i:
			sys.stderr.write (')\n')
			staffs.append ([])
			staff = staff + 1
			sys.stderr.write ('Staff %d ( ' % staff)
			i = get_voice (staff, s, staffs)
			if not i:
				del staffs[staff-1]
	return 0
	
staffs=[[]]
mup=files[0]
ly = os.path.basename (os.path.splitext (mup)[0]) + ".ly"
s = gulp_file (mup)
sys.stderr.write ('\n')
i=1
while i:
	i=get_staff (s, staffs)
sys.stderr.write ('\n')
sys.stderr.write ('Ly output to: %s...' % ly)
lyfile = open (ly, "w")
for i in range (len (staffs)):
	for v in range (len (staffs[i])):
		lyfile.write ("$staff%d_voice_%d = \\notes {\n %s\n}\n\n" % (i+1, v+1, staffs[i][v]))
lyfile.write ("\\score{\n")
lyfile.write ("\\notes <\n")
for i in range (len (staffs)):
	lyfile.write ("\\type Staff=staff%s <\n" % chr(ord('A')+i))
	for v in range (len (staffs[i])):
		lyfile.write ("{ \\$staff%d_voice_%d } " % (i+1, v+1))
	lyfile.write ("\n>\n")
lyfile.write (">\n")
lyfile.write ("\n}")
lyfile.close ()
sys.stderr.write ('\n')
