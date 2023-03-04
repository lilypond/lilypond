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


import lilylib as ly
import musicexp


def musicxml_step_to_lily(step):
    if step:
        return (ord(step) - ord('A') + 7 - 2) % 7
    else:
        return None


class Marker(musicexp.Music):
    def __init__(self):
        self.direction = 0
        self.event = None

    def print_ly(self, printer):
        ly.warning(_("Encountered unprocessed marker %s\n") % self)
        pass

    def ly_expression(self):
        return ""


class RepeatMarker(Marker):
    def __init__(self):
        Marker.__init__(self)
        self.times = 0


class EndingMarker(Marker):
    pass
