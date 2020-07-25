# text2html.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 1999--2020  Han-Wen Nienhuys <hanwen@xs4all.nl>
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
import re
import sys


entities = {
    "&": 'amp',
    "`": 'apos',
    '>': 'gt',
    '<': 'lt',
    '"': 'quot',
}


def txt2html(s):
    for i in list(entities.keys()):
        s = re.sub(i, '\001' + entities[i] + ';', s)
    s = re.sub('\001', '&', s)
    return s


for a in sys.argv[1:]:
    # hmm, we need: text2html out/foe.txt -> out/foe.html,
    # -o is a bit overkill?
    # outfile = os.path.basename (os.path.splitext(a)[0]) + '.html'
    outfile = os.path.splitext(a)[0] + '.html'

    try:
        os.unlink(outfile)
    except:
        pass

    s = r"""

<html>
<head>
 <META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
</head>

<body><pre>
%s
</pre></body></html>
""" % txt2html(open(a).read())
    open(outfile, 'w').write(s)
