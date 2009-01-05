#!/usr/bin/env python
# texi-skeleton-update.py

import sys
import glob
import os
import shutil

sys.stderr.write ('texi-skeleton-update.py\n')

orig_skeletons = set ([os.path.basename (f) for f in glob.glob (sys.argv[1] + '/*.ite??')])
new_skeletons = set ([os.path.basename (f) for f in glob.glob (sys.argv[2] + '/*.ite??')])

for f in new_skeletons:
    if f in orig_skeletons:
        g = open (os.path.join (sys.argv[1], f), 'r').read ()
        if '-- SKELETON FILE --' in g:
            sys.stderr.write ("Updating %s...\n" % f)
            shutil.copy (os.path.join (sys.argv[2], f), sys.argv[1])
    elif f != 'fdl.itexi':
        sys.stderr.write ("Copying new file %s...\n" % f)
        shutil.copy (os.path.join (sys.argv[2], f), sys.argv[1])

for f in orig_skeletons.difference (new_skeletons):
    sys.stderr.write ("Warning: outdated skeleton file %s\n" % f)
