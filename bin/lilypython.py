#!@PYTHON@

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
import sys
import os
import getopt




version_re = regex.compile('\\version *\"\(.*\)\"')
make_assign_re = regex.compile('^\([A-Z_]*\) *= *\(.*\)$')

def version_tuple(file):
    lines = file.readlines()

    mi = pa = mj = 0
    mp = ''
    
    for l in lines:
	if make_assign_re.search(l) <> -1:
	    nm = make_assign_re.group(1)
	    val = make_assign_re.group(2)
	    if nm == 'TOPLEVEL_MAJOR_VERSION':
		mj = atoi(val)
	    elif nm == 'TOPLEVEL_MINOR_VERSION':
		mi = atoi(val)
	    elif nm == 'TOPLEVEL_PATCH_LEVEL':
		pa = atoi(val)
	    elif nm == 'TOPLEVEL_MY_PATCH_LEVEL':
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
    return '%d.%d.%d%s' % tup

class Lilydirs:
    def __init__(self):
	try:
	    self.topdir = os.environ['LILYPOND_SOURCEDIR'] + '/'
	except IndexError:
	    self.topdir = os.environ['HOME'] + 'musix/current'
	    
	self.release_dir = self.topdir + '../releases/'
	self.patch_dir = self.topdir + '../patches/'

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

    print version_str_to_tuple(mv)
