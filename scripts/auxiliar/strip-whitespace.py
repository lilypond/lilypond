#!/usr/bin/env python

# This script removes trailing whitespace from files.
# It doesn't remove trailing newlines.
# As a side-effect, it converts line endings to Unix-style (LF).

import os
import sys

# Iterate through all arguments.  When the script is called
# with a wildcard (for example 'remove-trailing-whitespace.py *'),
# it's the *shell* that will expand the wildcard, and pass all
# resulting paths as arguments to the script.

for pathname in sys.argv[1:]:
    if os.path.isfile(pathname):
        fd = open(pathname, mode='U')  # open in universal newline mode
        lines = []
        for line in fd.readlines():
            lines.append(line.rstrip())
        fd.close()

        fd = open(pathname, mode='w')
        fd.seek(0)
        for line in lines:
            fd.write(line+'\n')
        fd.close()
