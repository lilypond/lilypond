# compile.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2020--2022  Jonas Hahnfeld <hahnjo@hahnjo.de>
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

from importlib.util import cache_from_source
import os.path
import py_compile
import sys

if len(sys.argv) != 3:
    print('Usage: compile.py <module.py> <outdir>')
    sys.exit(1)

src = sys.argv[1]
outdir = sys.argv[2]

# Compile src file, but store result in current directory.

cfile = cache_from_source(os.path.basename(src))
cfile = os.path.join(outdir, cfile)
py_compile.compile(src, cfile=cfile, doraise=True)
