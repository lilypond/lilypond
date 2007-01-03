#!/bin/sh
import os
import glob
import re
import sys
import optparse

#File 'accidental-engraver.cc'
#Lines executed:87.70% of 252

def summary (args):
    results = []
    for f in args:
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



def read_gcov (f):
    ls = []

    in_lines = [l for l in open (f).readlines ()]
    (count_len, line_num_len) = tuple (map (len, in_lines[0].split( ':')[:2]))
    
    for l in in_lines:
        c = l[:count_len].strip ()
        l = l[count_len+1:]
        n = int (l[:line_num_len].strip ())

        if n == 0:
            continue
        
        l = l[line_num_len+1:]

        ls.append ((c,n,l))
        
    return ls

def get_chunks (ls):
    chunks = []
    chunk = []
    for (c,n,l) in ls:
        if '#' in c:
            chunk.append ((n,l))
        elif c.strip () != '-' or l == '}\n':
            if chunk:
                chunks.append (chunk)
                chunk = []
            
    return chunks

def is_exception_chunk (ch):
    for (n,l) in ch:
        for stat in  ('warning', 'error'):
            if stat in l:
                return True
    return False

def print_chunk (ch, lines):
    nums = [n-1 for (n, l) in ch]
    for (c, n, l) in lines[min (nums):max (nums)+1]:
        sys.stdout.write ('%8s:%8d:%s' % (c,n,l))


def extract_uncovered (file):
    try:
        ls = read_gcov (file)
    except IOError, s :
        print s
        return
        
    cs = get_chunks (ls)
    cs = [c for c in cs if not is_exception_chunk (c)]
    print '\n'.join (['%d' % x for (a,x,b) in ls])
    for c in cs:
        print 'Uncovered chunk in', file
        print_chunk (c, ls)
        print '\n\n: '
    

def main ():
    p = optparse.OptionParser (usage="usage coverage.py [options] files",
                               description="")
    p.add_option ("--summary",
                  action='store_true',
                  default=False,
                  dest="summary")
    
    p.add_option ("--uncovered",
                  default=False,
                  action='store_true',
                  dest="uncovered")

    
    (options, args) = p.parse_args ()
    

    if options.summary:
        summary (['%s.gcov-summary' % s for s in args])

    if options.uncovered:
        for a in args:
            extract_uncovered ('%s.gcov' % a)
        
        
if __name__ == '__main__':
    main ()
