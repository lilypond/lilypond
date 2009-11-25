#! @PYTHON@

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2009 Jan Nieuwenhuizen <janneke@gnu.org>
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

import datetime
import os
import re
import sys
#
sys.path.insert (0, '@top-src-dir@/scripts/build')
import pytt

dry_run = False

def read_pipe (cmd, ignore_errors=False):
    pipe = os.popen (cmd)
    val = pipe.read ()
    if pipe.close () and not ignore_errors:
        raise SystemFailed ('Pipe failed: %(cmd)s' % locals ())
    return val

def filter_out (p, lst):
    return filter (lambda x: not p (x), lst)

copied_files = [
    'help2man.pl',
    'mf2pt1.mp',
    'mf2pt1.pl',
    'texinfo.tex',
    'txi-de.tex',
    'txi-en.tex',
    'txi-fr.tex',
    'txi-es.tex',
    ]

def main ():
    files = filter_out (lambda x: (os.path.basename (x) in copied_files
                                   or 'CHANGES' in x or 'ChangeLog' in x),
                        read_pipe ('cd @top-src-dir@ && git ls-files').split ())
    os.chdir ('@top-src-dir@')
    year = datetime.datetime.now ().year
    last_year = year - 1
    last_year_1d = last_year % 10
    for f in files:
        pytt.pytt ('(Copyright|\(c\)|\(C\)|@copyright\{\})\s*%(last_year)s([^-]|$)' % locals (),
                   r'\1 %(last_year)s--%(year)s' % locals (),
                   f)
        pytt.pytt ('(Copyright|\(c\)|\(C\)|@copyright\{\})\s*([^-]*)--(20[0-9][0-%(last_year_1d)s])' % locals (),
                   r'\1 \2--%(year)s' % locals (),
                   f)

if __name__ == '__main__':
    main ()
