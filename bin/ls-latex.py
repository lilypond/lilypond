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
    return  header

def print_html_head (l,o,h):
    (pre, ext) = o
    out = regsub.gsub ('\.[^.]+$', ext, h.filename)
    l.write ('<li><a href=%s>%s</a><p>by %s</p>' % (pre + out, h.title, h.author ))
    l.write ('</li>')


import getopt

(cl_options, files) = getopt.getopt(sys.argv[1:], 
				    'e:h', ['help', 'prefix=' ,'extension='])

tex = ''
output =''
pre = ''

for opt in cl_options:
    o = opt[0]
    a = opt[1]
    if o == '--extension' or o == '-e':
	ext = a
    if o == '--prefix' or o == '-p':
	pre = a

l = sys.stdout

l.write ('<html><title>TeX documents</title><h1> TeX documents</h1><ul>')

for x in files:
    print_html_head (l, (pre,ext), read_latex_header (x))
l.write ('</ul></html>')
