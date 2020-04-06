#!@PYTHON@

from importlib.util import cache_from_source
from os.path import basename
import py_compile
import sys

if len (sys.argv) != 2:
    print ('Usage: compile.py <module.py>')
    sys.exit (1)

src = sys.argv[1]

# Compile src file, but store result in current directory.

cfile = cache_from_source (basename(src))
py_compile.compile (src, cfile=cfile, doraise=True)
