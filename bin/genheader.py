#!@PYTHON@

# 
# genheader.py -- do headers (like these) 
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
# 



import posix
import pwd
import regex
import regsub
import string
import sys
import os
import getopt
import time
lilypath =''
try:
    lilypath = os.environ['LILYPOND_SOURCEDIR'] + '/'
except KeyError:
    print 'Please set LILYPOND_SOURCEDIR to the toplevel source, eg LILYPOND_SOURCEDIR=/home/foobar/lilypond-1.2.3/'
    sys.exit(1)

lilypath = lilypath + '/bin/'
sys.path.append(lilypath)

from lilypython import *

class My_options:
    def __init__(self):
	self.commentify = None
	self.add_hdr_def = 0
	self.classname = ''

my_options = My_options()


def name():
    return pwd.getpwuid(posix.getuid())[4]

def c_commentify(str):
    return  '/* ' + regsub.gsub('^','  ', str) + '\n */';

def sh_commentify(str):
    return regsub.gsub('^', '# ', str)

def tex_commentify(str):
    return regsub.gsub('^', '% ', str)

def project_str():
    cwd = posix.getcwd()
    if regex.search('flower', cwd) <> -1:
	PROJECT = "the Flower Library"
    elif regex.search('mf$', cwd) <> -1:
	PROJECT = "the Feta (defintively not an abbreviation for Font-En-Tja) music font"
    else:
	PROJECT = "the GNU LilyPond music typesetter"
    return PROJECT

def head_str(filename):
    if my_options.add_hdr_def:
	what = "declare " 
    else:
	what=" implement "


    headstr = '\n%s -- %s\n\nsource file of %s\n\n(c) %d %s <%s>\n' \
	      %(filename, what, project_str(), time.localtime (time.time ())[0], name(), mailaddress())
    return headstr


def c_include(filename):
    startdef= filename;
    trans = string.maketrans( string.lowercase + '-.', string.uppercase + '__')
    startdef = string.translate(filename, trans)

   
    headstr = "\n\n#ifndef %s\n#define %s\n" % (startdef, startdef)
    terminatestr = "#endif /* %s */\n"  % (startdef);

    return headstr+ '\n\n'+ terminatestr;





(options, files) = getopt.getopt(sys.argv[1:], 'tcsh', ['class']) 

for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '-c':
	my_options.commentify = c_commentify
    elif o == '-t':
	my_options.commentify = tex_commentify
    elif o == '-s':
	my_options.commentify = sh_commentify
    elif o == '-h':
	my_options.add_hdr_def = 1
    elif o == '--class':
	my_options.classname = a



#
# FIXME:  should create xxx.cc and include/xxx.hh, with implement/declare Xxx
# in  one run
if my_options.classname:
    pass
	
def do_file(nm):
    s = my_options.commentify(head_str(nm)) 
    if my_options.add_hdr_def:
	s = s + c_include(nm)
    return s


def extension(ext,nm):
    ext = '\\.' + ext
    return regex.search(ext, nm) <> -1

def c_extension(nm):
    return extension('hh',nm) or extension('cc',nm) \
	   or extension('icc', nm) or extension('tcc',nm)
def select_commentification(nm):
    if c_extension (nm):
	return c_commentify
    elif extension('py',nm) or extension('pl',nm) or extension('sh',nm):
	return  sh_commentify
    elif extension('mf',nm) or extension('tex',nm) or extension('ly',nm):
	return tex_commentify
    else: 
	raise 'help'

for nm in files:
    if extension('hh', nm) or extension('icc', nm) or  extension('tcc', nm): 
	my_options.add_hdr_def = 1
    if my_options.commentify == None:
	my_options.commentify = select_commentification(nm)
    print do_file(nm)
