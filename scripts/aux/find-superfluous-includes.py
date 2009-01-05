#!/usr/bin/env python
import sys
import re
import os


full_paths = {}
incs = {}
inc_re = re.compile ('^#include "([^"]+)"')
def parse_file (fn):
    lst = []

    lc = 0
    for l in open (fn).readlines():
        lc += 1
        m = inc_re.search (l)
        if m:
            lst.append ((lc, m.group (1)))

    base = os.path.split (fn)[1]
    full_paths[base] = fn
    incs[base] = lst
    

def has_include (f, name):
    try:
        return name in [b for (a,b) in incs[f]]
    except KeyError:
        return False

for a in sys.argv:
    parse_file (a)

print '-*-compilation-*-'
for (f, lst) in incs.items ():
    for (n, inc) in lst:
        for (n2, inc2) in lst:
            if has_include (inc2, inc):
                print "%s:%d: already have %s from %s" % (full_paths[f], n,
                                                          inc, inc2)
                break

        
    
