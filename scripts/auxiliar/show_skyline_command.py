# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2012--2020 Joe Neeman <joeneeman@gmail.com>
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

# A gdb plugin debugging skylines.  To use the plugin, make sure that
# skyline_viewer.py is in your PATH, then add
# source /path/to/show_skyline_command.py
# to your .gdbinit file.  The 'vsky' and 'hsky' commands for
# drawing skylines will then be available in gdb.

import gdb
from subprocess import Popen, PIPE
from math import isinf

SKYLINE_VIEWER = 'skyline_viewer.py'

# Function taken from GCC


def find_type(orig, name):
    typ = orig.strip_typedefs()
    while True:
        search = str(typ) + '::' + name
        try:
            return gdb.lookup_type(search)
        except RuntimeError:
            pass
        # The type was not found, so try the superclass.    We only need
        # to check the first superclass, so we don't bother with
        # anything fancier here.
        field = typ.fields()[0]
        if not field.is_base_class:
            raise ValueError("Cannot find type %s::%s" % (str(orig), name))
        typ = field.type

# Class adapted from GCC


class ListIterator:
    def __init__(self, val):
        self.val = val
        self.nodetype = find_type(val.type, '_Node')
        self.nodetype = self.nodetype.strip_typedefs().pointer()

        head = val['_M_impl']['_M_node']
        self.base = head['_M_next']
        self.head = head.address

    def __iter__(self):
        return self

    def __next__(self):
        if self.base == self.head:
            raise StopIteration
        elt = self.base.cast(self.nodetype).dereference()
        self.base = elt['_M_next']
        return elt['_M_data']


def to_list(list_val):
    return list(ListIterator(list_val))


def skyline_to_lines(sky_value):
    """Turns a gdb.Value into a list of line segments."""
    sky_d = int(sky_value['sky_'])
    buildings = to_list(sky_value['buildings_'])

    def bld_to_line(bld):
        y_intercept = float(bld['y_intercept_']) * sky_d
        slope = float(bld['slope_']) * sky_d
        x1 = float(bld['start_'])
        x2 = float(bld['end_'])

        if isinf(y_intercept) or isinf(x1) or isinf(x2):
            return None
        return (x1, y_intercept + slope * x1, x2, y_intercept + slope * x2)

    ret = list(map(bld_to_line, buildings))
    return [r for r in ret if r is not None]


viewer = Popen(SKYLINE_VIEWER, stdin=PIPE)


def add_skyline(lines):
    global viewer
    try:
        for line in lines:
            x1, y1, x2, y2 = line
            viewer.stdin.write('(%f,%f) (%f,%f)\n' % (x1, y1, x2, y2))
        viewer.stdin.write('\n')
    except IOError:
        # If the pipe is broken, it probably means that someone closed
        # the viewer window. Open another one.
        viewer = Popen(SKYLINE_VIEWER, stdin=PIPE)
        add_skyline(lines)


class ShowSkylineCommand (gdb.Command):
    "Show a skyline graphically."

    def __init__(self, command_name):
        super(ShowSkylineCommand, self).__init__(command_name,
                                                 gdb.COMMAND_DATA,
                                                 gdb.COMPLETE_SYMBOL, False)

    def to_lines(self, skyline):
        pass

    def invoke(self, arg, from_tty):
        global plot

        val = gdb.parse_and_eval(arg)
        typ = val.type

        # If they passed in a reference or pointer to a skyline,
        # dereference it.
        if typ.code == gdb.TYPE_CODE_PTR or typ.code == gdb.TYPE_CODE_REF:
            val = val.referenced_value()
            typ = val.type

        if typ.tag == 'Skyline_pair':
            sky = val['skylines_']
            arr = sky['array_']
            add_skyline(self.to_lines(arr[0]))
            add_skyline(self.to_lines(arr[1]))

        elif typ.tag == 'Skyline':
            add_skyline(self.to_lines(val))


class ShowVSkylineCommand (ShowSkylineCommand):
    "Show a vertical skyline."

    def __init__(self):
        super(ShowVSkylineCommand, self).__init__("vsky")

    def to_lines(self, skyline):
        return skyline_to_lines(skyline)


class ShowHSkylineCommand (ShowSkylineCommand):
    "Show a horizontal skyline."

    def __init__(self):
        super(ShowHSkylineCommand, self).__init__("hsky")

    def to_lines(self, skyline):
        lines = skyline_to_lines(skyline)
        # Because it is a horizontal skyline, reflect around the line y=x.
        return [(y1, x1, y2, x2) for (x1, y1, x2, y2) in lines]


ShowHSkylineCommand()
ShowVSkylineCommand()
