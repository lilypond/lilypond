# -*- coding: utf-8 -*-
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2016--2023 John Gourlay <john@weathervanefarm.net>
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

import lilylib as ly
import musicexp


def musicxml_step_to_lily(step):
    if step:
        return (ord(step) - ord('A') + 7 - 2) % 7
    else:
        return None


def musicxml_numbers_to_volte(number_string):
    res = []

    # Volta numbers are separated by commas, optionally followed by a space
    # (XML extraction already removed leading and trailing whitespace, also
    # squeezing sequences of whitespace characters inbetween into single
    # spaces).
    #
    # An empty string is also valid input, but the return value is still an
    # empty list.
    numbers = re.split(r', ?', number_string)
    for number in numbers:
        # According to the standard, no leading zeroes are allowed.  We
        # ignore this restriction, simply using `int` for the conversion to
        # integers.
        try:
            n = int(number)
        except ValueError:
            return []

        if n > 0:
            res.append(n)
        else:
            return []
    return res


class Marker(musicexp.Music):
    def __init__(self):
        self.direction = 0

    def print_ly(self, printer):
        ly.warning(_("Encountered unprocessed marker %s\n") % self)
        pass

    def ly_expression(self):
        return ""


class RepeatMarker(Marker):
    def __init__(self):
        Marker.__init__(self)
        self.times = 2  # A simple repeat played twice as the default.
        self.at_start = False


class EndingMarker(Marker):
    def __init__(self):
        Marker.__init__(self)
        self.mxl_event = None
        self.volte = []


class RepeatEndingMarker(RepeatMarker, EndingMarker):
    def __init__(self, repeat, ending):
        self.direction = repeat.direction
        # According to the standard, `times` is not used if there is an
        # `<ending>` element at the same time.
        self.times = None
        self.at_start = repeat.at_start
        self.mxl_event = ending.mxl_event
        self.volte = ending.volte
