#!/usr/bin/python

# 
# lily-python.py --  implement general LilyPond-wide python stuff
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
# 

import posix
import pwd
import regex
import regsub
from string import *
from flower import *
import sys
import os
import getopt




version_re = regex.compile('\\version *\"\(.*\)\"')
# now used as shell script in configure too!
# make_assign_re = regex.compile('^\([A-Z_]*\) *= *\(.*\)$')
make_assign_re = regex.compile('^\([A-Z_]*\)=\(.*\)$')

def version_tuple(file):
    lines = file.readlines()

    mi = pa = mj = 0
    mp = ''
    
    for l in lines:
	if make_assign_re.search(l) <> -1:
	    nm = make_assign_re.group(1)
	    val = make_assign_re.group(2)
#	    if nm == 'TOPLEVEL_MAJOR_VERSION':
	    if nm == 'MAJOR_VERSION':
		mj = atoi(val)
#	    elif nm == 'TOPLEVEL_MINOR_VERSION':
	    elif nm == 'MINOR_VERSION':
		mi = atoi(val)
#	    elif nm == 'TOPLEVEL_PATCH_LEVEL':
	    elif nm == 'PATCH_LEVEL':
		pa = atoi(val)
#	    elif nm == 'TOPLEVEL_MY_PATCH_LEVEL':
	    elif nm == 'MY_PATCH_LEVEL':
		mp = val
    return (mj,mi,pa,mp)

def next_version(tup):
    return (tup[0], tup[1], tup[2] + 1, tup[3]);

def prev_version(tup):
    t = tup
    if t[3]:
	return (tup[0], tup[1], tup[2], '');
    elif t[2] == 0 :
	return (tup[0], tup[1] -1, tup[2], '');	
    else:	
	return (tup[0], tup[1], tup[2] - 1, '');


def dirname(v):
    return 'lilypond-' + version_tuple_to_str(v)

def tarball(v):
    return dirname(v)  + '.tar.gz'

def released_tarball(v):
    return lilydirs.release_dir + tarball(v)


def tuple_to_list(tup):
    l=[]
    for x in tup:
	l.append[x]
    return l

def version_str_to_tuple(str):
    t = split(str, '.')
    try:
	mypatch = t[3]
    except IndexError:
	mypatch = ''
	
    return (atoi(t[0]), atoi(t[1]), atoi(t[2]), mypatch)



def guess_mudela_version(filename):
    f = open (filename)
    lines = f.readlines()
    f.close()
    for l in lines:
	if version_re.search(l) <> -1:
	    return version_re.group(1)
    
    return ''

def version_tuple_to_str(tup):
    mypatch =''
    if tup[3]:
	mypatch = '.' + tup[3]
    
    return ('%d.%d.%d' % tup[0:3]) + mypatch

class Lilydirs:
    def __init__(self):
	try:
	    self.topdir = os.environ['LILYPOND_SOURCEDIR'] + '/'
	
	except KeyError:
	    print 'Please set LILYPOND_SOURCEDIR to the toplevel source, eg LILYPOND_SOURCEDIR=/home/foobar/lilypond-1.2.3/'
	    sys.exit(1)
	    
	try:
	    self.groupdir = os.environ['LILYPOND_GROUPDIR'] + '/'
	except KeyError:
	    self.groupdir = self.topdir + '../'
 
	self.release_dir = self.groupdir + '/releases/'
	self.patch_dir = self.groupdir + '/patches/'

    def version_tuple(self):
        f = open (self.topdir + 'VERSION')
	v = version_tuple(f)
	f.close ()
	return v



lilydirs = Lilydirs()

if __name__ == '__main__':
    v= lilydirs.version_tuple()
    print v, prev_version(v), next_version(v)
    mv =  guess_mudela_version(lilydirs.topdir + 'init/symbol.ly')
    pv=(0,1,1,'jcn4')
    print version_tuple_to_str(pv), prev_version(pv), next_version(pv)
    print version_tuple_to_str((0,1,1,''))    
    print mv, version_str_to_tuple(mv)


    
def dump_file(f, s):
    i = open(f, 'w')
    i.write(s)
    i.close ()

def gulp_file(f):
    i = open(f)
    i.seek (0, 2)
    len = i.tell ()
    i.seek (0,0)
    return i.read (len)


header_regex = regex.compile('\\header[ \t\n]*{\([^}]*\)}')
header_entry_regex = regex.compile('[\n\t ]*\([^\n\t ]+\)[\n\t ]*=[\n \t]*\([^;]+\)[\n \t]*;')

#
# FIXME breaks on multiple strings.
#
def read_mudela_header (fn):
    s = gulp_file(fn)
    s = regsub.gsub('%.*$', '', s)
    s = regsub.gsub('\n', ' ', s)    

    dict = {}
    if header_regex.search(s) <> -1:
	h = header_regex.group(1)
    else:
	return dict

    while regex.search('=', h) <> -1: 

	if header_entry_regex.search (h) == -1:

	    raise 'format error'

	h = regsub.sub(header_entry_regex, '', h)
	left = header_entry_regex.group(1)
	right = header_entry_regex.group(2)

	right = regsub.gsub('\([^\\]\)\"', '\\1', right)
	right = regsub.gsub('^"', '', right)	
	left = regsub.gsub('\([^\\]\)\"', '', left)
	left = regsub.gsub('^"', '', left)

	dict[left] = right

    return dict
   
	
