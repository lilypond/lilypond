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

format_names  = {'ps.gz': 'Compressed PostScript',
		 'html' : 'HTML'
		 }

class Latex_head:
    def __init__ (self):
	self.author = ''
	self.title = ''
	self.date = ''
	self.format = ''
	
def read_latex_header (s):
    header = Latex_head()
    m = re.search(r'\\author{([^}]+)}', s)
    if m:
	header.author = m.group (1)

    m = re.search (r'\\title{([^}]+)}',s )
    if m:
	header.title = m.group (1)

    header.formats = ['ps.gz']
    return  header


def read_bib_header (s):

    m = re.search ('% *AUTHOR *= *(.*)\n',s)

    header = Latex_head()

    if m:
	header.author = m.group (1)

    m = re.search ('% *TITLE *= *(.*)\n',s )
    if m:
	header.title = m.group (1)

    header.formats = ['html']
    return header


def read_pod_header (s):
    header = Latex_head ()

    i = re.search( '[^\n \t]', s)
    s = s[i:]
    i = re.search( '\n\n', s)
    s = s[i+2:]    
    i = re.search( '\n\n', s)
    header.title = s[:i]

    return  header

def read_texinfo_header (s):
    header = Latex_head ()

    m = re.search( '@settitle (.*$)', s)
    if m:
	header.title = m.group (1)

    header.formats = ['html', 'ps.gz']
    return header



def print_html_head (l,o,h):
    pre =o

    
    fn = pre + h.basename

    t = h.filename 
    if h.title :
	t = t + ': '+ h.title

    l.write ('<li>%s </a>' % t)

    if h.author:
	l.write ('<p>by %s</p>' % h.author)

    for f in h.formats:
	l.write ('(<a href=%s.%s>%s</a>)' % (fn, f, format_names [f]))
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

l.write (r"""<html><title>%s</title>
<body>
<h1> %s</h1><ul>
""" % (title, title))


read_header_funcs = {
    'pod' : read_pod_header,
    'tex' : read_latex_header,
    'doc' : read_latex_header,
    'bib': read_bib_header, 
    'latex' : read_latex_header,
    'tely' : read_texinfo_header,
    'texi': read_texinfo_header,
}    


for x in files:
    m = re.search ('\\.([^.]*)$', x)
    if m == None:
	continue

    s = gulp_file (x)
    head = read_header_funcs [m.group(1)] (s)

    head.filename = x
    head.basename = re.sub ("\\.[^.]+", '', x)
    
    print_html_head (l, pre, head)

l.write ('</ul></body></html>')

