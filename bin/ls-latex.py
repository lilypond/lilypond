#!@PYTHON@

import sys
import os

lilypath =''
try:
    lilypath = os.environ['LILYPOND_SOURCEDIR'] + '/'
except KeyError:
    print 'Please set LILYPOND_SOURCEDIR to the toplevel source, eg LILYPOND_SOURCEDIR=/home/foobar/lilypond-1.2.3/'
    sys.exit(1)

lilypath = lilypath + '/bin/'
sys.path.append(lilypath)
 
from lilypython import *
import __main__
import glob



latex_author_re = regex.compile('\\author{\([^}]+\)}')
latex_title_re = regex.compile('\\title{\([^}]+\)}')

class Latex_head:
    def __init__ (self):
	self.author = ''
	self.title = ''
	self.date = ''
	self.site = ''
    

def read_latex_header (fn):
    s = gulp_file (fn)
    i = regex.search( '\\\\begin{document}', s)
    if i < 0:
	raise 'huh?'
    s = s[:i]
    s = regsub.gsub('%.*$', '', s)
    s = regsub.gsub('\n', ' ', s)
    if latex_author_re.search (s) == -1 :
	raise 'huh?'

    header = Latex_head()
    header.filename= fn;
    header.author = latex_author_re.group (1)
    if latex_title_re.search (s) == -1:
	raise 'huh?'
    header.title = latex_title_re.group (1)
    header.outfile = regsub.gsub ('\.doc+$', '.ps.gz', fn)
    return  header


bib_author_re = regex.compile('% *AUTHOR *= *\(.*\)$')
bib_title_re = regex.compile('% *TITLE *= *\(.*\)$')

def bib_header (fn):
    s = gulp_file (fn)
    if bib_author_re.search (s) == -1 :
	raise 'huh?'

    header = Latex_head()
    header.filename= fn;
    header.author = bib_author_re.group (1)
    if bib_title_re.search (s) == -1:
	raise 'huh?'    
    header.title = bib_title_re.group (1)
    header.outfile = regsub.gsub ( '\.bib$', '.html' , fn)
    return header


def read_pod_header (fn):
    header = Latex_head ()
    s = gulp_file (fn)
    i = regex.search( '[^\n \t]', s)
    s = s[i:]
    i = regex.search( '\n\n', s)
    s = s[i+2:]    
    if i < 0:
	raise 'huh?'
    i = regex.search( '\n\n', s)
    header.title = s[:i]
    header.filename = fn
    header.outfile = regsub.gsub ('\.pod$', '.html', fn)
    return  header


def print_html_head (l,o,h):
    pre =o

    l.write ('<li><a href=%s>%s</a>' % (pre + h.outfile, h.title ))
    if h.author:
	l.write ('<p>by %s</p>' % h.author)
    l.write ('</li>\n')


import getopt

(cl_options, files) = getopt.getopt(sys.argv[1:], 
				    'e:h', ['help', 'prefix=' 
					    , 'title='])

tex = ''
output =''
pre = ''
title = ''
for opt in cl_options:
    o = opt[0]
    a = opt[1]
    if o == '--prefix' or o == '-p':
	pre = a
    if o == '--title' or o == '-t':
	title = a  

l = sys.stdout

l.write ('<html><title>%s</title><h1> %s</h1><ul>\n' % (title, title))


for x in files:
    if regex.search ('\\.bib$', x) <> -1:
	head = bib_header (x)
    elif regex.search ('\\.pod$', x) <> -1:
	head = read_pod_header (x)
    else:
	head = read_latex_header (x)
    print_html_head (l, pre, head)
l.write ('</ul></html>')
