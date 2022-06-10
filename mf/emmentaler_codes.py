# emmentaler_codes.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2022  Werner Lemberg <wl@gnu.org>
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


"""
Unicode code points for LilyPond's Emmentaler Font
"""


import fontforge


# Some LilyPond glyphs make sense in 'text mode', i.e., their metrics
# are similar to metrics of other text glyphs like 'm' or 'zero'.
# This file assigns Unicode values to all such glyphs that don't have
# names conforming to the Adobe Glyph List (AGL), and which FontForge
# thus can't handle automatically.

def add_code_points(font):
    def add(name, code):
        glyph = font[font.findEncodingSlot(name)]
        glyph.unicode = code

    add("accidentals.doublesharp.figbass", 0x1D12A) # MUSICAL SYMBOL DOUBLE SHARP
    add("accidentals.flat.figbass", 0x266D)         # MUSIC FLAT SIGN
    add("accidentals.flatflat.figbass", 0x1D12B)    # MUSICAL SYMBOL DOUBLE FLAT
    add("accidentals.natural.figbass", 0x266E)      # MUSIC NATURAL SIGN
    add("accidentals.sharp.figbass", 0x266F)        # MUSIC SHARP SIGN

# eof
