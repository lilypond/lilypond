#!@PYTHON@

# template.py -- 
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1998 

program_name = 'template'
version = '0.1'

import os
import sys

sys.path.append ('@abs-step-bindir@')
sys.path.append (os.environ['HOME'] + '/usr/src/lilypond/stepmake/bin')

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
    sys.stdout.write ("Usage: %s [options] [files]\n"
                      "I'm not a program, use me as a template to create one\n\n"
                      + "Options:\n"
                      + "  -h, --help             print this help\n"
                      % (program_name)
		      )
    sys.exit (0)

identify ()
(options, files) = getopt.getopt (
    sys.argv[1:], 'hp:', ['help', 'package'])
for opt in options:
    o = opt[0]
    a = opt[1]
    if o== '--help' or o == '-h':
	help ()
    elif o == '-p' or o == '--package':
	topdir = a
    else:
	print o
	raise getopt.error

sys.path.append (topdir + '/stepmake/bin')
from packagepython import *
package = Package (topdir)
packager = Packager ()

from flower import *

