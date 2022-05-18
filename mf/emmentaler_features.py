# emmentaler_features.py
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
OpenType font features for LilyPond's Emmentaler Font
"""


import fontforge


# Note that font feature tags always consist of four letters;
# friendlier names than 'ss01' or 'ss02' that might be easier to
# remember are not available, alas.
#
# The OpenType features should be present regardless of script and
# language; we thus use the 'DFLT' script and 'dflt' (dummy) language
# tags.


# Stylistic set 'ss01' selects glyphs for figured bass.

def add_feature_ss01(font):
    font.addLookup("ss01", "gsub_single", 0,
                   (("ss01",
                     (("DFLT",
                       ("dflt")), )), ))
    font.addLookupSubtable("ss01", "figuredBass")

    def ss01(glyph, variant):
        font[glyph].addPosSub("figuredBass", variant)

    ss01("zero", "figbass.zero")
    ss01("one", "figbass.one")
    ss01("two", "figbass.two")
    ss01("three", "figbass.three")
    ss01("four", "figbass.four")
    ss01("five", "figbass.five")
    ss01("six", "figbass.six")
    ss01("seven", "figbass.seven")
    ss01("eight", "figbass.eight")
    ss01("nine", "figbass.nine")


# Stylistic set 'ss02' selects glyphs for fingering.

def add_feature_ss02(font):
    font.addLookup("ss02", "gsub_single", 0,
                   (("ss02",
                     (("DFLT",
                       ("dflt")), )), ))
    font.addLookupSubtable("ss02", "fingering")

    def ss02(glyph, variant):
        font[glyph].addPosSub("fingering", variant)

    ss02("four", "fingering.four")
    ss02("seven", "fingering.seven")

# eof
