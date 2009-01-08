#! /usr/bin/python

'''
    Copyright (c) 2008--2009
    Jan Nieuwenhuizen <janneke@gnu.org>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
'''

import os
import re
import stat
import sys

dry_run = False

def pytt (from_re, to, file_name):
    s = file (file_name).read ()
    name = os.path.basename (file_name)
    base, ext = os.path.splitext (name)
    t = re.sub (from_re, to % locals (), s)
    if s != t:
        if dry_run:
            sys.stdout.write (t)
        else:
            stat_info = os.stat (file_name)
            mode = stat.S_IMODE (stat_info[stat.ST_MODE])
            os.system ('mv --backup=t %(file_name)s %(file_name)s~' % locals ())
            file (file_name, 'w').write (t)
            os.chmod (file_name, mode)

def main ():
    from_re = re.compile (sys.argv[1], re.MULTILINE)
    to = sys.argv[2]
    if not sys.argv[3:] or sys.argv[3] == '-':
        sys.stdout.write (re.sub (from_re, to, sys.stdin.read ()))
    else:
        for f in sys.argv[3:]:
            pytt (from_re, to, f)
    
if __name__ == '__main__':
    main ()
