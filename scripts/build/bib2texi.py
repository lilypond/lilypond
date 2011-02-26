#!@PYTHON@
import os
import sys
import getopt
import tempfile

# usage:
def usage ():
    print 'usage: %s [-s style] [-o <outfile>] BIBFILES...'

(options, files) = getopt.getopt (sys.argv[1:], 's:o:', [])

output = 'bib.itexi'
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

tmpfile = tempfile.mkstemp ('bib2texi')[1]

#This writes a .aux file to the temporary directory.
#The .aux file contains the commands for bibtex
#PEH changed the bibstyle to allow a single template file in the parent directory
#The template filename is texi-*.bst, where * defaults to 'long' but can be a parameter
open (tmpfile + '.aux', 'w').write (r'''
\relax
\citation{*}
\bibstyle{%(style)s}
\bibdata{%(files)s}''' % vars ())

tmpdir = tempfile.gettempdir ()

#The command line to invoke bibtex
cmd = "TEXMFOUTPUT=%s bibtex %s" % (tmpdir, tmpfile)

sys.stdout.write ("Invoking `%s'\n" % cmd)
#And invoke it
stat = os.system (cmd)
if stat <> 0:
    sys.exit(1)

#TODO: do tex -> itexi on output
# Following 2 lines copy tmpfile.bbl to the desired output file
bbl = open (tmpfile + '.bbl').read ()

open (output, 'w').write  (bbl)

def cleanup (tmpfile):
    for a in ['aux','bbl', 'blg']:
        os.unlink (tmpfile + '.' + a)


cleanup (tmpfile)
#Following line added by PEH - script was leaving a dangling temporary file with no extension
os.unlink (tmpfile)

