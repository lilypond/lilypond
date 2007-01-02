#!/bin/sh
import os
import glob
import re

os.chdir ('out-cov')

#File 'accidental-engraver.cc'
#Lines executed:87.70% of 252

results = []
for f in glob.glob ('*.gcov-summary'):
    str = open (f).read ()
    m = re.search ("File '([^']+.cc)'\s*Lines executed:([0-9.]+)% of ([0-9]+)", str)

    if m and '/usr/lib' in m.group (1):
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

print 'files sorted by number of untested lines (decreasing)'
print
print '%5s (%6s): %s' % ('cov %', 'lines', 'file')
print '----------------------------------------------'

for (pain, d) in results:
    print '%(cov)5.2f (%(lines)6d): %(file)s' % d

