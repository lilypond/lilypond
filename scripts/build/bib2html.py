#!@PYTHON@
import os
import sys
import getopt
import tempfile

# usage:
def usage ():
    print 'usage: %s [-s style] [-o <outfile>] BIBFILES...'

(options, files) = getopt.getopt (sys.argv[1:], 's:o:', [])

output = 'bib.html'
style = 'long'

for (o,a) in options:
    if o == '-h' or o == '--help':
        usage ()
        sys.exit (0)
    elif o == '-s' or o == '--style':
        style = a
    elif o == '-o' or o == '--output':
        output = a
    else:
        raise Exception ('unknown option: %s' % o)


if style not in ['alpha','index','long','longp','long-pario','short','short-pario','split']:
    sys.stderr.write ("Unknown style \`%s'\n" % style)

tempfile = tempfile.mktemp ('bib2html')

if not files:
   usage ()
   sys.exit (2)


def strip_extension (f, ext):
    (p, e) = os.path.splitext (f)
    if e == ext:
        e = ''
    return p + e

nf = []
for f in files:
    nf.append (strip_extension (f, '.bib'))

files = ','.join (nf)

open (tempfile + '.aux', 'w').write (r'''
\relax 
\citation{*}
\bibstyle{html-%(style)s}
\bibdata{%(files)s}''' % vars ()) 

cmd = "bibtex %s" % tempfile

sys.stdout.write ("Invoking `%s'\n" % cmd)
stat = os.system (cmd)
if stat <> 0:
    sys.exit(1)


#TODO: do tex -> html on output 

bbl = open (tempfile + '.bbl').read ()

open (output, 'w').write  (bbl)


def cleanup (tempfile):
    for a in ['aux','bbl', 'blg']:
        os.unlink (tempfile + '.' + a)

cleanup (tempfile)

