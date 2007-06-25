#!@PYTHON@
# mass-link.py

# USAGE:  mass-link.py  [--prepend-suffix SUFFIX]   symbolic | hard   SOURCEDIR DESTDIR FILES
#
# create hard or symbolic links to SOURCEDIR/FILES in DESTDIR
#
# if --prepend-suffix is specified, link to foo.bar will be called fooSUFFIX.bar
# shell-wildcard expansion is performed on FILES.

print "mass_link.py"

import sys
import os
import glob
import getopt

optlist, args = getopt.getopt (sys.argv[1:], '', ['prepend-suffix='])
link_type, source_dir, dest_dir = args[0:3]
files = args[3:]

prepended_suffix = ''
for x in optlist:
    if x[0] == '--prepend-suffix':
        prepended_suffix = x[1]

if prepended_suffix:
    def insert_suffix (p):
        l = p.split ('.')
        if len (l) >= 2:
            l[-2] += prepended_suffix
            return '.'.join (l)
        return p + prepended_suffix
else:
    insert_suffix = lambda p: p

if link_type == 'symbolic':
    link = os.symlink
elif link_type == 'hard':
    link = os.link
else:
    sys.stderr.write(sys.argv[0] + ': ' + link_type + ": wrong argument, expected 'symbolic' or 'hard'\n")
    sys.exit (1)

sourcefiles = []
for pattern in files:
    sourcefiles += (glob.glob (os.path.join (source_dir, pattern)))

destfiles = map (lambda f: os.path.join (dest_dir, insert_suffix (os.path.basename (f))), sourcefiles)

def force_link (src,dest):
    if os.path.exists (dest):
        os.system ('rm -rf ' + dest)
    link (src, dest)

map (force_link, sourcefiles, destfiles)
