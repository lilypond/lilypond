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
import string
import sys
import os
import getopt





make_assign_re = regex.compile('^\([A-Z_]*\) *= *\(.*\)$')

def version_str_tuple(file):
    lines = file.readlines()

    mi = pa = mp = mj = ''
    
    for l in lines:
	if make_assign_re.search(l) <> -1:
	    nm = make_assign_re.group(1)
	    val = make_assign_re.group(2)
	    if nm == 'TOPLEVEL_MAJOR_VERSION':
		mj = val
	    elif nm == 'TOPLEVEL_MINOR_VERSION':
		mi = val
	    elif nm == 'TOPLEVEL_PATCH_LEVEL':
		pa = val
	    elif nm == 'TOPLEVEL_MY_PATCH_LEVEL':
		mp = val
    return (mj,mi,pa,mp)

class Lilydirs:
    def __init__(self):
	try:
	    self.topdir = os.environ['LILYPOND_SOURCEDIR'] + '/'
	except IndexError:
	    self.topdir = os.environ['HOME'] + 'musix/current'
	    
	self.release_dir = self.topdir + '../releases/'
	self.patch_dir = self.topdir + '../patches/'

    def version_str_tuple(self):
        f = open (self.topdir + 'VERSION')
	v = version_str_tuple(f)
	f.close ()
	return v

lilydirs = Lilydirs()

print lilydirs.version_str_tuple()
