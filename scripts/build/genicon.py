# genicon.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2006--2020  Han-Wen Nienhuys <hanwen@xs4all.nl>
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

import os
import sys
import tempfile

base = os.path.splitext(os.path.split(sys.argv[1])[1])[0]
input = os.path.abspath(sys.argv[1])
output = os.path.abspath(sys.argv[2])
program_name = os.path.split(sys.argv[0])[1]

dir = tempfile.mktemp(program_name)
os.mkdir(dir, 0o777)
os.chdir(dir)


def system(c):
    print(c)
    if os.system(c):
        raise Exception('The command exited with nonzero exit status!')


outputs = []
for sz in [48, 32, 16]:

    for depth in [24, 8]:
        out = '%(base)s-%(sz)d-%(depth)d.png' % locals()
        system('convert -depth %(depth)d -sample %(sz)d %(input)s %(out)s' %
               locals())
        outputs.append(out)

system('icotool --output %s --create %s' % (output, ' '.join(outputs)))
system('rm -rf %(dir)s' % locals())
