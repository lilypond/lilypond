#!@PYTHON@

# 
# table-to-html.py -- convert char-separated table to html table
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1998 Jan Nieuwenhuizen <jan@digicash.com>
# 

import getopt
from string import *
import regex
import regsub
import os
import sys
import time

version = '0.1'

lilypath =''
try:
	lilypath = os.environ['LILYPOND_SOURCEDIR'] + '/'
except KeyError:
	try:
		lilypath = os.environ['top_srcdir'] + '/'
	except KeyError:
	    print 'Please set LILYPOND_SOURCEDIR to the toplevel source, eg LILYPOND_SOURCEDIR=/home/foobar/lilypond-1.2.3/'

lilypath = lilypath + '/bin/'
sys.path.append (lilypath)
 
from flower import *

def program_id ():
    return 'table-to-html.py version ' + version;

def identify ():
    sys.stdout.write (program_id () + '\n')
    
def help ():
    sys.stdout.write ("Usage: table-to-html [options] TABLE_FILE HTML_FILE\n"
		 + "Generate mozarella metrics table from preparated feta log\n\n"
		 + "Options:\n"
		 + "  -h, --help             print this help\n"
		 + "  -s, --separator=SEP    specify separator [:]\n")
    sys.exit (0)


def header (html):
    html.write ('<body><table cellspacing=10>')

def footer (html):
    html.write ('</table></body>')

def convert (inname, outname, separator):
    table = File (inname)
    # ugh
    html = File (outname, 'w')

    header (html)
    while not table.eof ():
	line = table.readline ()
	columns = split (line, separator)
	html_line = '<tr><td>' + join (columns, '</td><td>') + '</td></tr>'
	html.write (html_line)
    table.close ()
    footer (html)
    html.close ()


def main ():
    identify ()
    (options, files) = getopt.getopt (
	sys.argv[1:], 'hs:', ['help','separator='])

    separator = ':'
    for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--separator' or o == '-s':
	    separator = a
	elif o== '--help' or o == '-h':
	    help ()
	else:
	    print o
	    raise getopt.error

    convert (files[0], files[1], separator)

main ()

