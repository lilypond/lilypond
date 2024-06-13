# -*- coding: utf-8 -*-
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2024 Werner Lemberg <wl@gnu.org>
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


import musicexp

# Use a global variable to store the settings needed inside a `\layout`
# block.  Whenever we need to change a setting or add/remove an engraver, we
# can access this variable and add the corresponding settings.
layout_information = musicexp.Layout()

# Use a global variable to store the settings needed inside a `\paper`
# block.
paper = musicexp.Paper()
