#!@PYTHON@

# table-to-html.py -- convert char-separated table to html table
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>

version = '0.1'
name = 'table-to-html'

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
    sys.stdout.write ("Usage: table-to-html [OPTION]... TABLE-FILE HTML-FILENAME\n"
		 + "Generate pretty table from char separated table\n\n"
		 + "Options:\n"
		 + "  -h, --help             print this help\n"
		 + "  -p, --package=DIR      specify package\n"
		 + "  -s, --separator=SEP    specify separator [:]\n"
		 + "  -t, --latex            do latex output instead\n"
		      )
    
    sys.exit (0)


def header (html):
    html.write ('<body bgcolor=white><table cellspacing=10>')

def footer (html):
    html.write ('</table></body>')

def convert_html (inname, outname, separator):
    # urg, again?
    from flower import *
    table = File (inname)
    # ugh
    html = File (outname, 'w')

    header (html)
    i = 0
    while not table.eof ():
	line = table.readline ()
	i = i + 1
	if not len(line):
	    continue
	columns = split (line, separator)
	html_line = '<tr><td>' + join (columns, '</td><td>') + '</td></tr>'
	html.write (html_line)
	if len (columns) < 7:
	    print inname + ': ' + str(i) + ':warning: not enough cols\n'
	    continue
	if len (columns) > 7:
	    print inname + ': ' + str(i) + ':warning: too many cols\n'
	    continue

    table.close ()
    footer (html)
    html.close ()

def convert_tex (inname, outname, separator):
    # urg, again?
    from flower import *
    table = File (inname)
    # ugh
    html = File (outname, 'w')

    i = 0
    while not table.eof ():
	line = table.readline ()
	i = i + 1
	if not len(line):
	    continue
	columns = split (line, separator)
	if len (columns) < 7:
	    print inname + ': ' + str(i) + ':warning: not enough cols\n'
	    continue
	if len (columns) > 7:
	    print inname + ': ' + str(i) + ':warning: too many cols\n'
	    continue

	html_line =  '\\tableentry{' + join (columns, '}{') + '}\n'
	html.write (html_line)
    table.close ()
    html.close ()

def main ():
    identify ()
    (options, files) = getopt.getopt (
	sys.argv[1:], 'to:hp:s:', ['help', 'latex', 'output=', 'package=', 'separator='])
    latex = 0
    separator = ':'
    output = ''
    for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--separator' or o == '-s':
	    separator = a
	elif o== '--help' or o == '-h':
	    help ()
	elif o=='--latex' or o == '-t':
	    latex = 1
	elif o == '--output' or o == '-o':
	    output = a
	elif o == '--package' or o == '-p':
	    topdir=a
	else:
	    print o
	    raise getopt.error

    sys.path.append (topdir + '/stepmake/bin')
    from packagepython import *
    package = Package (topdir)
    packager = Packager ()

    from flower import *

    if latex:
	convert_tex (files[0], output, separator)
    else:
	convert_html (files[0], output, separator)

main ()

