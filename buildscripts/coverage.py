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

class Chunk:
    def __init__ (self, range, all_lines, file):
        self.range = range
        self.all_lines = all_lines
        self.file = file

    def length (self):
        return self.range[1] - self.range[0]
    def text (self):
        return ''.join ([l[0] for l in self.lines()])
        
    def lines (self):
        return self.all_lines[self.range[0]:
                              self.range[1]]
    def widen (self):
        self.range = (min (self.range[0] -1, 0),
                      self.range[0] +1)
    def write (self):
        print 'uncovered chunk in', self.file
        for (c, n, l) in self.lines ():
            sys.stdout.write ('%8s:%8d:%s' % (c,n,l))
            
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

def get_chunks (ls, file):
    chunks = []
    chunk = []
    for (c,n,l) in ls:
        if '#' in c:
            chunk.append ((n,l))
        elif c.strip () != '-' or l == '}\n':
            if chunk:
                nums = [n-1 for (n, l) in chunk]
                chunks.append (Chunk ((min (nums), max (nums)+1),
                                      ls, file))
                chunk = []
            
    return chunks


def widen_chunk (ch, ls):
    a -= 1
    b += 1

    return [(n, l)  for (c, n, l) in ls[a:b]]
    

def is_exception_chunk (ch):
    for (n,l) in ch:
        for stat in  ('warning', 'error'):
            if stat in l:
                return True
    return False

def is_inspection_chunk (ch):
    for (n,l) in ch:
        for stat in  ('::print',):
            if stat in l:
                return True
    return False

def extract_uncovered (file):
    try:
        ls = read_gcov (file)
    except IOError, s :
        print s
        return []
        
    cs = get_chunks (ls, file)
    def interesting (c):
        t = c.text()
        for stat in  ('warning', 'error', 'print'):
            if stat in t:
                return False
        return True

   
    return [c for c in cs if interesting (c)]
    

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
        uncovered = []
        for a in args:
            uncovered += extract_uncovered ('%s.gcov' % a)
            
        uncovered = [(c.length (), c) for c in uncovered]
        uncovered.sort ()
        uncovered.reverse ()
        for (score, c) in uncovered:
            c.write ()

            
        
if __name__ == '__main__':
    main ()
