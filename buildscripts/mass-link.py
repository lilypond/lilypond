#!@PYTHON@
# mass-link.py

# USAGE:  mass-link.py   symbolic | hard   SOURCEDIR DESTDIR FILES
#
# create hard or symbolic links to SOURCEDIR/FILES in DESTDIR
#
# shell-wildcard expansion is performed on FILES.

print "mass_link.py"

import sys
import os
import glob

link_type, source_dir, dest_dir = sys.argv[1:4]
files = sys.argv[4:]

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

destfiles = map (lambda f: os.path.join (dest_dir, os.path.basename (f)), sourcefiles)

def force_link (src,dest):
    if os.path.exists (dest):
        os.system ('rm -rf ' + dest)
    link (src, dest)

map (force_link, sourcefiles, destfiles)
