#!/usr/bin/env python

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

# A GTK+ program for debugging skylines. The program reads a sequence
# of line segments from stdin (one line segment per line of stdin, in the format
# '(x1, y1) (x2, y2)'). A skyline is terminated by an empty line, which
# causes the skyline to be displayed on the screen.

from threading import Thread
from math import isinf
import gtk
import gobject
import goocanvas
import sys
import re


class GtkSkylineCanvas (goocanvas.Canvas):
    """A Canvas for displaying skylines."""

    def __init__(self):
        super(GtkSkylineCanvas, self).__init__()
        self.connect('size-allocate', GtkSkylineCanvas.rescale)
        self.x_min = float('inf')
        self.x_max = float('-inf')
        self.y_min = float('inf')
        self.y_max = float('-inf')

        self.colors = ('black', 'red', 'green', 'blue',
                       'maroon', 'olive', 'teal')
        self.cur_color_index = 0

    def rescale(self, allocation):
        width = (self.x_max - self.x_min + 1) * 1.1
        height = (self.y_max - self.y_min + 1) * 1.1
        if width <= 0 or height <= 0:
            return

        scale_x = allocation.width / width
        scale_y = allocation.height / height
        scale = min(scale_x, scale_y)
        self.set_scale(scale)

        center_x = (self.x_max + self.x_min) / 2
        center_y = (self.y_max + self.y_min) / 2
        actual_width = allocation.width / scale
        actual_height = allocation.height / scale
        actual_min_x = center_x - actual_width / 2
        actual_max_x = center_x + actual_width / 2
        actual_min_y = center_y - actual_height / 2
        actual_max_y = center_y + actual_height / 2

        self.set_bounds(actual_min_x, actual_min_y, actual_max_x, actual_max_y)
        self.scroll_to(actual_min_x, actual_min_y)

    def add_skyline(self, lines):
        """Adds a skyline to the current canvas, in a new color.

        The canvas will be rescaled, if necessary, to make room for the
        new skyline."""
        # Flip vertically, because goocanvas thinks higher numbers are
        # further down, while lilypond thinks they're further up.
        lines = [(x1, -y1, x2, -y2) for (x1, y1, x2, y2) in lines]

        color = self.colors[self.cur_color_index]
        self.cur_color_index = (self.cur_color_index + 1) % len(self.colors)

        # Update the bounding box of the skylines.
        x_vals = [s[0] for s in lines] + [s[2] for s in lines]
        y_vals = [s[1] for s in lines] + [s[3] for s in lines]
        self.x_min = min([self.x_min] + x_vals)
        self.x_max = max([self.x_max] + x_vals)
        self.y_min = min([self.y_min] + y_vals)
        self.y_max = max([self.y_max] + y_vals)

        # Add the lines to the canvas.
        root = self.get_root_item()
        for (x1, y1, x2, y2) in lines:
            goocanvas.polyline_new_line(root, x1, y1, x2, y2,
                                        stroke_color=color,
                                        line_width=0.05)
        self.rescale(self.get_allocation())

# We want to run the gtk main loop in a separate thread so that
# the main thread can be responsible for reading stdin.


class SkylineWindowThread (Thread):
    """A thread that runs a Gtk.Window displaying a skyline."""

    def run(self):
        gtk.gdk.threads_init()
        self.window = None
        self.canvas = None
        gtk.main()

    # This should only be called from the Gtk main loop.
    def _destroy_window(self, window):
        sys.exit(0)

    # This should only be called from the Gtk main loop.
    def _setup_window(self):
        if self.window is None:
            self.window = gtk.Window()
            self.canvas = GtkSkylineCanvas()
            self.window.add(self.canvas)
            self.window.connect("destroy", self._destroy_window)
            self.window.show_all()

    # This should only be called from the Gtk main loop.
    def _add_skyline(self, lines):
        self._setup_window()
        self.canvas.add_skyline(lines)

    def add_skyline(self, lines):
        # Copy the lines, just in case someone modifies them.
        gobject.idle_add(self._add_skyline, list(lines))


thread = SkylineWindowThread()
thread.setDaemon(True)
thread.start()


def lines(infile):
    line = infile.readline()
    while len(line) > 0:
        yield line[:-1]
        line = infile.readline()


point_re_str = r'\(([a-z.0-9-]*) *,([a-z0-9.-]*)\)'
line_re_str = point_re_str + r' +' + point_re_str
line_re = re.compile(line_re_str)

# The main loop just reads lines from stdin and feeds them to the
# display.
current_skyline = []
for line in lines(sys.stdin):
    if not line:
        thread.add_skyline(current_skyline)
        current_skyline = []
        continue

    m = re.search(line_re, line)
    if m is None:
        print('line did not match')
    else:
        pts = list(map(float, m.groups()))
        if not any(map(isinf, pts)):
            current_skyline.append(pts)
