#!@PYTHON@

# update.py -- update current source tree
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>

program_name = 'update'
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
    return program_name + ' version ' + version;

def identify ():
    sys.stdout.write (program_id () + '\n')

def help ():
    sys.stdout.write ("Usage: %s [OPTION]...\n"
                      "Update sourcetree\n\n"
                      + "Options:\n"
		      + "  -f, --file=FILE      specify patch file\n"
                      + "  -h, --help           print this help\n"
		      + "  -p, --package=DIR    specify package\n"
		      + "  -v, --version=VER    specify patch version\n"
                      % (program_name)
		      )
    sys.exit (0)

identify ()
(options, files) = getopt.getopt (
    sys.argv[1:], 'f:hp:v:', ['file=', 'help', 'package=', 'version='])
patch=''
ver=''
for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '--help' or o == '-h':
	help ()
    elif o == '-f' or o == '--file':
	patch = a
    elif o == '-p' or o == '--package':
	topdir = a
    elif o == '-v' or o == '--version':
	ver = a
    else:
	print o
	raise getopt.error

sys.path.append (topdir + '/stepmake/bin')
from packagepython import *
package = Package (topdir)
packager = Packager ()

from flower import *

def read_patch_vector (patch):
    vec = []
    pipe = os.popen ('gzip -dc ' + patch)
    line = pipe.readline ()
    while line and line != '--state\n':
	line = pipe.readline ()
    line = pipe.readline ()
    while line and line != '++state\n':
	vec.append (line[:len (line)-1])
	line = pipe.readline ()
    pipe.close ()
    return vec 
    
def read_state_vector (states):
    vec = []
    file = File (states)
    while not file.eof ():
	line = file.readline ()
	if line:
	    vec.append (line[:len (line)-1])
    return vec

def read_relevant_state_vector (states, from_str):
    vec = read_state_vector (states)
    for i in range (len (vec)):
	if vec[i] == from_str:
	    return vec[i:]
    return []

def find_revert (states, patch):
    for i in range (len (state_vector)):
	for j in doubles:
	    if j in state_vector[:i+1]:
		return state_vector[i:]
    return []

if patch == '' and ver != '':
    patch = package.patch_dir + '/' + package.name + '-%s.diff.gz' % ver
if patch == '':
    if 0:
	files = os.listdir (package.patch_dir)
	patches = []
	for i in files:
	    if regex.search (package.name + '-.*.diff.gz', i) == 0:
		patches.append (i)
	# urg: sort
	patch = package.patch_dir + '/' + patches[len (patches) -1]
    else:
	os.chdir (package.patch_dir)
	pipe = os.popen ('/bin/ls -t1 ' + package.name 
	    + '-*.diff.gz 2> /dev/null')
	patch = pipe.readline ()
	patch = patch[:len (patch) -1]
	pipe.close ()
	if not patch:
	    raise 'patch not found'
	patch = package.patch_dir + '/' + patch
	print patch

os.chdir (package.topdir)
patch_vector = read_patch_vector (patch)
print 'patch vector: ' + str (patch_vector)
from_str = patch_vector[0]
state_vector = read_relevant_state_vector ('make/STATE-VECTOR', from_str)
print 'relevant state vector: ' + str (state_vector)

doubles = []
for i in patch_vector[1:]:
    if i in state_vector:
	doubles.append (i)
print 'doubles: ' + str (doubles)

revert = find_revert (state_vector, patch_vector)
redo = []
for i in revert:
    redo.append (i)
revert.reverse ()

for i in doubles:
    redo.remove (i)

if revert or redo or doubles:
    print 'not smart enough; please do:'
    print '  * revert: ' + str (revert)
    print '  * apply: ' + os.path.basename (patch)
    print '  * redo: ' + str (redo)
    sys.exit (1)

status = os.system ('echo "gzip -dc %s | patch -p1 -E --force"' % patch)
if status:
    raise 'apply patch failed'

sys.stdout.write ('checking...')
rejects = my_find (['*.rej'], '.')
if len (rejects):
    print 'rejects found:'
    for i in rejects:
	print i
    sys.exit (1)
print 'ok'
sys.stdout.write ('cleaning...')
origs = my_find (['*.orig'], '.')
for i in origs:
    os.remove (i)
print 'ok'
