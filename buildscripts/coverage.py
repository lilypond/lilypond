#!/usr/bin/python

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
    def __init__ (self, range, coverage_count, all_lines, file):
        assert coverage_count >= 0
        assert type (range) == type (())
        
        self.coverage_count = coverage_count
        self.range = range
        self.all_lines = all_lines
        self.file = file

    def length (self):
        return self.range[1] - self.range[0]

    def text (self):
        return ''.join ([l[2] for l in self.lines()])
        
    def lines (self):
        return self.all_lines[self.range[0]:
                              self.range[1]]
    def widen (self):
        self.range = (min (self.range[0] -1, 0),
                      self.range[0] +1)
    def write (self):
        print 'chunk in', self.file
        for (c, n, l) in self.lines ():
            cov = '%d' % c
            if c == 0:
                cov = '#######'
            elif c < 0:
                cov = ''
            sys.stdout.write ('%8s:%8d:%s' % (cov, n, l))
            
    def uncovered_score (self):
        return self.length ()
    
class SchemeChunk (Chunk):
    def uncovered_score (self):
        text = self.text ()
        if (text.startswith  ('(define ')
            and not text.startswith ('(define (')):
            return 0

        if text.startswith  ('(use-modules '):
            return 0

        if (text.startswith  ('(define-public ')
            and not text.startswith ('(define-public (')):
            return 0

        return len ([l for (c,n,l) in self.lines() if (c == 0)]) 

def read_gcov (f):
    ls = []

    in_lines = [l for l in open (f).readlines ()]
    (count_len, line_num_len) = tuple (map (len, in_lines[0].split (':')[:2]))
    
    for l in in_lines:
        c = l[:count_len].strip ()
        l = l[count_len+1:]
        n = int (l[:line_num_len].strip ())

        if n == 0:
            continue

        if '#' in c:
            c = 0
        elif c == '-':
            c = -1
        else:
            c = int (c)
        
        l = l[line_num_len+1:]

        ls.append ((c,n,l))
        
    return ls

def get_c_chunks (ls, file):
    chunks = []
    chunk = []

    last_c = -1
    for (c, n, l) in ls:
        if not (c == last_c or c < 0 and l != '}\n'):
            if chunk and last_c >= 0:
                nums = [n-1 for (n, l) in chunk]
                chunks.append (Chunk ((min (nums), max (nums)+1),
                                      last_c, ls, file))
                chunk = []

        chunk.append ((n,l))
        if c >= 0:
            last_c = c
            
    return chunks

def get_scm_chunks (ls, file):
    chunks = []
    chunk = []

    def new_chunk ():
        if chunk:
            nums = [n-1 for (n, l) in chunk]
            chunks.append (SchemeChunk ((min (nums), max (nums)+1),
                                        max (last_c, 0), ls, file))
            chunk[:] = []
        
    last_c = -1
    for (cov_count, line_number, line) in ls:
        if line.startswith ('('):
            new_chunk ()
            last_c = -1
        
        chunk.append ((line_number, line))
        if cov_count >= 0:
            last_c = cov_count

    return chunks

def widen_chunk (ch, ls):
    a -= 1
    b += 1

    return [(n, l)  for (c, n, l) in ls[a:b]]
    

def extract_chunks (file):
    try:
        ls = read_gcov (file)
    except IOError, s :
        print s
        return []
        
    cs = []
    if 'scm' in file:
        cs = get_scm_chunks (ls, file)
    else:
        cs = get_c_chunks (ls, file)
    return cs


def filter_uncovered (chunks):
    def interesting (c):
        if c.coverage_count > 0:
            return False
        
        t = c.text()
        for stat in  ('warning', 'error', 'print', 'scm_gc_mark'):
            if stat in t:
                return False
        return True
   
    return [c for c in chunks if interesting (c)]
    

def main ():
    p = optparse.OptionParser (usage="usage coverage.py [options] files",
                               description="")
    p.add_option ("--summary",
                  action='store_true',
                  default=False,
                  dest="summary")
    
    p.add_option ("--hotspots",
                  default=False,
                  action='store_true',
                  dest="hotspots")
    
    p.add_option ("--uncovered",
                  default=False,
                  action='store_true',
                  dest="uncovered")

    
    (options, args) = p.parse_args ()
    

    if options.summary:
        summary (['%s.gcov-summary' % s for s in args])

    if options.uncovered or options.hotspots:
        chunks = []
        for a in args:
            name = a
            if name.endswith ('scm'):
                name += '.cov'
            else:
                name += '.gcov'
            
            chunks += extract_chunks  (name)

        if options.uncovered:
            chunks = filter_uncovered (chunks)
            chunks = [(c.uncovered_score (), c) for c in chunks if c.uncovered_score() > 0]
        elif options.hotspots:
            chunks = [((c.coverage_count, -c.length()), c) for c in chunks]
            
            
        chunks.sort ()
        chunks.reverse ()
        for (score, c) in chunks:
            c.write ()

            
        
if __name__ == '__main__':
    main ()
