#!@PYTHON@

from importlib.util import cache_from_source
import os.path
import py_compile
import sys

if len (sys.argv) != 3:
    print ('Usage: compile.py <module.py> <outdir>')
    sys.exit (1)

src = sys.argv[1]
outdir = sys.argv[2]

# Compile src file, but store result in current directory.

cfile = cache_from_source (os.path.basename (src))
cfile = os.path.join (outdir, cfile)
py_compile.compile (src, cfile=cfile, doraise=True)
