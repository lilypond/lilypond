# mf-to-table.py -- convert spacing info in MF logs .

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 1997--2023 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
import time


def read_log_file(fn):
    str = open(fn, encoding='utf-8').read()
    str = re.sub('[\n\r]', '', str)
    str = re.sub('[\t ]+', ' ', str)

    autolines = []

    def auto_func(match, a=autolines):
        a.append(match.group(1))
        return ''

    str = re.sub('@{(.*?)@}', auto_func, str)
    return autolines


class Char_metric:
    def __init__(self):
        pass


def parse_logfile(fn):
    autolines = read_log_file(fn)
    charmetrics = []

    global_info = {}
    group = ''

    for i, l in enumerate(autolines):
        tags = l.split('@:')
        if tags[0] == 'group':
            group = tags[1]
        elif tags[0] == 'puorg':
            group = ''
        elif tags[0] == 'char':
            try:
                name = tags[12]
            except IndexError:
                print('Error in mf-to-table while processing file', fn)
                print('Index 12 >', len(tags) - 1, 'on line', i)
                print(l)
                raise

            if group:
                name = group + '.' + name
            m = {
                'description': tags[1],
                'name': name,
                'breapth': float(tags[2]),
                'width': float(tags[3]),
                'depth': float(tags[4]),
                'height': float(tags[5]),
                'wx': float(tags[6]),
                'wy': float(tags[7]),
                'dwx': float(tags[8]),
                'dwy': float(tags[9]),
                'accbot': float(tags[10]),
                'acctop': float(tags[11]),
            }
            charmetrics.append(m)
        elif tags[0] == 'font':
            global_info['design_size'] = float(tags[4])

        elif tags[0] == 'parameter':
            global_info[tags[1]] = tags[2]

    return (global_info, charmetrics)


def character_lisp_table(global_info, charmetrics):

    def conv_char_metric(charmetric):
        f = 1.0

        s = '(%s .' % charmetric['name']
        s += ('\n ((bbox . (%.3f %.3f %.3f %.3f))' %
              (-charmetric['breapth'] * f, -charmetric['depth'] * f,
               charmetric['width'] * f, charmetric['height'] * f))
        if (charmetric['name'].startswith('noteheads.')):
            s += ('\n  (attachment . (%.3f . %.3f))' %
                  (charmetric['wx'], charmetric['wy']))
            s += ('\n  (attachment-down . (%.3f . %.3f))' %
                  (charmetric['dwx'], charmetric['dwy']))
        elif (charmetric['name'].startswith('accidentals.')):
            s += ('\n  (ledger-shortening-range . (%.3f . %.3f))' %
                  (charmetric['accbot'], charmetric['acctop']))
        s += '))\n'

        return s

    s = ''
    for c in charmetrics:
        s += conv_char_metric(c)

    return s


def global_lisp_table(global_info):
    str = ''

    keys = ['staffsize', 'stafflinethickness', 'staff_space',
            'linethickness', 'black_notehead_width', 'ledgerlinethickness',
            'design_size',
            'blot_diameter'
            ]
    for k in keys:
        if k in global_info:
            str = str + "(%s . %s)\n" % (k, global_info[k])

    return str


for name in sys.argv[1:]:
    root, _ = os.path.splitext(name)
    global_lisp_nm = root + '.global-lisp'
    char_lisp_nm = root + '.lisp'

    g, m = parse_logfile(name)
    open(char_lisp_nm, 'w', encoding='utf-8').write(character_lisp_table(g, m))
    open(global_lisp_nm, 'w', encoding='utf-8').write(global_lisp_table(g))
