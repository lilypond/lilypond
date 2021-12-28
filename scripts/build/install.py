# install.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

import getopt
import sys
import os
import shutil

(opts, args) = getopt.getopt(sys.argv[1:], 'b:cdg:m:o:qst:', [])

transform_base = None
group = None
owner = None
transform = None
mode = None
copy = False
create_dir = False
quiet = False

for (o, a) in opts:
    if o == '-b':
        transform_base = a
    elif o == '-c':
        copy = True
    elif o == '-d':
        create_dir = True
    elif o == '-g':
        group = a
    elif o == '-m':
        mode = int(a, base=8)
    elif o == '-o':
        owner = a
    elif o == '-q':
        quiet = True
    elif o == '-s':
        strip = True
    elif o == '-t':
        transform = a
    elif o == '-h':
        print(''' Usage: $0 [OPTION]... SRCFILE DSTFILE
 or: $0 [OPTION]... SRCFILES... DIRECTORY
 or: $0 -d DIRECTORIES...

In the first form, install SRCFILE to DSTFILE, removing SRCFILE by default.
In the second, create the directory path DIR.

Options:
-b=TRANSFORMBASENAME
-c         copy source (using $cpprog) instead of moving (using $mvprog).
-d         create directories instead of installing files.
-g GROUP   $chgrp installed files to GROUP.
-m MODE    $chmod installed files to MODE.
-o USER    $chown installed files to USER.
-q         don't emit messages.
-s         strip installed files (using $stripprog).
-t=TRANSFORM
--help     display this help and exit.
--version  display version info and exit.''')
        sys.exit(0)

if not mode:
    if create_dir:
        mode = 0o755
    else:
        mode = 0o644

chown_me = []

dest = None
if not create_dir:
    dest = args.pop()
    if not quiet:
        sys.stdout.write("install.py: target directory `%s':\n" % dest)

for f in args:
    if create_dir:
        if os.path.isdir(f):
            continue

        if not quiet:
            sys.stdout.write("install.py: creating directory `%s'\n" % f)
        os.makedirs(f, mode=mode, exist_ok=True)
        chown_me.append(f)
    else:
        if copy:
            if os.path.exists(dest) and not os.path.isdir(dest):
                os.remove(dest)
            shutil.copy2(f, dest)
            if not quiet:
                sys.stdout.write("install.py:   copying file `%s'\n" % f)
        else:
            shutil.move(f, dest)
            if not quiet:
                sys.stdout.write("install.py:   moving file `%s'\n" % f)

        if os.path.isdir(dest):
            chown_me.append(os.path.join(dest, os.path.basename(f)))
        else:
            chown_me.append(dest)

for f in chown_me:
    os.chmod(f, mode)
    if group is not None or owner is not None:
        os.chown(f, group, owner)
