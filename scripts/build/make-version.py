# make-version.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 1999--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
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

import re
import sys

PROGRAM = sys.argv[0]
VERSION = sys.argv[1]
defs = []
for i in open(VERSION, encoding='utf8').readlines():
    i = re.sub('#.*', '', i)
    m = re.search('([^ =]*)[\t ]*=[ \t]*([^ \t]*)[ \t]*\n', i)
    if m:
        defs.append((m.group(1), m.group(2)))

sys.stdout.write(r'''
/*
 Automatically generated from %(VERSION)s
 by %(PROGRAM)s.
*/
#ifndef VERSION_HH
#define VERSION_HH
''' % vars())

for name, expansion in defs:
    # GUILE leaks autoconf data into userspace.
    sys.stdout.write(r'''
#ifdef %(name)s
#undef %(name)s
#endif /* %(name)s */
#define %(name)s "%(expansion)s"
''' % vars())

if ('MY_PATCH_LEVEL', '') in defs:
    sys.stdout.write(r'''
#define NO_MY_PATCHLEVEL
#define TOPLEVEL_VERSION MAJOR_VERSION "." MINOR_VERSION "." PATCH_LEVEL
''')
else:
    sys.stdout.write(r'''
#define TOPLEVEL_VERSION MAJOR_VERSION "." MINOR_VERSION "." PATCH_LEVEL "." MY_PATCH_LEVEL
''')

sys.stdout.write(r'''
#endif /* VERSION_HH */
''')
