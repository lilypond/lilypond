#!@PYTHON@

# name isn't really appropriate now ...

name = 'ls-latex'
version = '0.1'

import sys
import os
import string

def gulp_file (fn):
	f = open (fn)
	return f.read ()

import __main__
import glob

import re

class Latex_head:
    def __init__ (self):
	self.author = ''
	self.title = ''
	self.date = ''
	self.format = ''

	
def read_latex_header (fn):
    s = gulp_file (fn)
    header = Latex_head()
    m = re.search(r'\\author{([^}]+)}', s)
    if m:
	header.author = m.group (1)

    m = re.search (r'\\title{([^}]+)}',s )
    if m:
	header.title = m.group (1)
    else:
	header.title = 'No title'

    header.outfile = fn
    header.outfile = re.sub ('\.latex$', '.ps.gz', header.outfile)
    header.outfile = re.sub ('\.tex$', '.ps.gz', header.outfile)
    header.outfile = re.sub ('\.doc$', '.ps.gz', header.outfile)
    header.format = 'gzipped postscript'
    return  header


def bib_header (fn):
    s = gulp_file (fn)
    m = re.search ('% *AUTHOR *= *(.*)$',s)
    header = Latex_head()    
    if m:
	header.author = m.group (1)


    m = re.search ('% *TITLE *= *(.*)$',s )
    if m:
	header.title = m.group (1)
    else:
	header.title = '(bibliography without title)'

    header.outfile = re.sub ( '\.bib$', '.html' , fn)
    header.format = 'HTML'    
    return header


def read_pod_header (fn):
    header = Latex_head ()
    s = gulp_file (fn)
    i = re.search( '[^\n \t]', s)
    s = s[i:]
    i = re.search( '\n\n', s)
    s = s[i+2:]    
    if i < 0:
	sys.stderr.write ('gulped file: ' + fn + '\n')
	raise 'huh?'
    i = re.search( '\n\n', s)
    header.title = s[:i]
    header.filename = fn
    header.outfile = re.sub ('\.pod$', '.html', fn)
    return  header

def read_texinfo_header (fn):
    header = Latex_head ()
    s = gulp_file (fn)
    m = re.search( '@settitle (.*$)', s)
    if m:
	header.title = m.group (1)
    header.filename = fn
    header.outfile = re.sub ('\.tely', '.html', fn)
    header.format = 'HTML'
    return header

def read_tely_header (fn):
    header = Latex_head ()
    s = gulp_file (fn)
    m = re.search( '@settitle (.*$)', s)
    if m:
	header.title = m.group (1)
    header.filename = fn
    header.outfile = re.sub ('\.tely', '.html', fn)
    header.format = 'HTML'
    return header


def print_html_head (l,o,h):
    pre =o

    l.write ('<li><a href=%s>%s (%s)</a>' % (pre + h.outfile, h.title, h.format ))
    if h.author:
	l.write ('<p>by %s</p>' % h.author)
    l.write ('</li>\n')

def help ():
    sys.stdout.write ("Usage: ls-latex [OPTION]... FILE...\n"
		 "Generate html index file for FILE...\n\n"
		 + "Options:\n"
		 + "  -h, --help             print this help\n"
		      )
    sys.exit (0)

import getopt

(options, files) = getopt.getopt(sys.argv[1:], 
    'e:h', ['help', 'prefix=',  'title='])

tex = ''
output =''
pre = ''
title = ''
for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '--prefix':
	pre = a
    elif o == '--title':
	title = a  
    elif o == '-h' or o == '--help':
    	help ()


l = sys.stdout

l.write ('<html><title>%s</title><h1> %s</h1><ul>\n' % (title, title))


for x in files:
    if re.search ('\\.bib$', x) :
	head = bib_header (x)
    elif re.search ('\\.pod$', x) :
	head = read_pod_header (x)
    elif re.search ('\\.texinfo$', x) :
	head = read_texinfo_header (x)
    elif re.search ('\\.tely$', x):
	head = read_tely_header (x)
    else:
	head = read_latex_header (x)
    if head.title == '':
	head.title = head.filename
    print_html_head (l, pre, head)

l.write ('</ul></html>')

