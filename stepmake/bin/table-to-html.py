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

def convert_html (inname, outname, cols, separator, linesep):
    table = open (inname)
    # ugh
    html = open (outname, 'w')

    header (html)
    i = 0
    for line in table.readlines ():
	i = i + 1
	if not len(line):
	    continue
	columns = split (line, separator)
	html_line = '<tr><td>' + join (columns, '</td><td>') + '</td></tr>'
	html_line= regsub.gsub (linesep, ' ',html_line)
	html.write (html_line)

	if len (columns) <> cols:
		print i
		raise 'not enough cols'

    table.close ()
    footer (html)
    html.close ()


def convert_tex (inname, outname, cols, separator, linesep):
    table = open (inname)
    html = open(outname, 'w')

    i = 0
    for line in table.readlines ():    
	i = i + 1
	if not len(line):
	    continue
	columns = split (line, separator)
	if len (columns) <> cols:
		print i
		raise 'not enough cols'

	tex_line =  '\\tableentry{' + join (columns, '}{') + '}\n'
	tex_line = regsub.gsub (linesep, ' ', tex_line)
	html.write (tex_line)
	
    table.close ()
    html.close ()

def main ():
    identify ()
    (options, files) = getopt.getopt (
	sys.argv[1:], 'tl:o:hp:c:s:', ['columns=', 'help', 'latex', 'output=', 'package=', 'separator=', 'linesep='])
    latex = 0
    separator = '@'
    output = ''
    linesep = '\r'
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
	elif o == '--linesep' or o == '-l':
		linesep = a
	elif o == '--columns' or o == '-c':
		cols =  atoi(a)
	else:
	    print o
	    raise getopt.error

    sys.path.append (topdir + '/stepmake/bin')
    from packagepython import *
    package = Package (topdir)
    packager = Packager ()

    from flower import *

    if latex:
	convert_tex (files[0], output,  cols, separator, linesep)
    else:
	convert_html (files[0], output, cols, separator, linesep)

main ()

