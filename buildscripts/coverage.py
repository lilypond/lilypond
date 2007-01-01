#!/bin/sh
import os
import glob
import re

cmds = """
./configure --enable-config=cov --disable-optimising
make conf=cov -j2
make conf=cov test LILYPOND_JOBS=          

cd out-cov
ln ../lily/* .
ln ../lily/out-conv/*cc .
mkdir include
ln ../lily/include/* include/
for a in *[cyl] ; do  gcov -o ../lily/out-cov/  -p $a > $a.gcov-summary ; done 
"""

os.chdir ('out-cov')

#File 'accidental-engraver.cc'
#Lines executed:87.70% of 252

results = []
for f in glob.glob ('*.gcov-summary'):
    str = open (f).read ()
    m = re.search ("File '([^']+.cc)'\s*Lines executed:([0-9.]+)% of ([0-9]+)", str)
    if '/usr/lib' in m.group (1):
        continue
   
    if m:
        cov = float (m.group (2))
        lines = int (m.group (3))
        pain = lines * (100.0 - cov)
        file = m.group (1)
        tup = (pain, locals ().copy())
        
        results.append(tup)

results.sort ()
results.reverse()
for (pain, d) in results:
    print '%(cov)5.2f (%(lines)6d): %(file)s' % d

