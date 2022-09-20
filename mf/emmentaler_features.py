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
# friendlier names than 'ss01' or 'cv47' that might be easier to
# remember are not available on the Pango level, alas.
#
# The OpenType features should be present regardless of script and
# language; we thus use the 'DFLT' script and 'dflt' (dummy) language
# tags.


# Stylistic set 'ss01' selects fattened glyphs.

def add_feature_ss01(font):
    font.addLookup("ss01", "gsub_single", 0,
                   (("ss01",
                     (("DFLT",
                       ("dflt")), )), ))
    font.addLookupSubtable("ss01", "fattened")

    def ss01(glyph, variant):
        font[glyph].addPosSub("fattened", variant)

    ss01("zero", "fattened.zero")
    ss01("one", "fattened.one")
    ss01("two", "fattened.two")
    ss01("three", "fattened.three")
    ss01("four", "fattened.four")
    ss01("four.alt", "fattened.four.alt")
    ss01("five", "fattened.five")
    ss01("six", "fattened.six")
    ss01("seven", "fattened.seven")
    ss01("seven.alt", "fattened.seven.alt")
    ss01("eight", "fattened.eight")
    ss01("nine", "fattened.nine")

    ss01("fixedwidth.zero", "fattened.fixedwidth.zero")
    ss01("fixedwidth.one", "fattened.fixedwidth.one")
    ss01("fixedwidth.two", "fattened.fixedwidth.two")
    ss01("fixedwidth.three", "fattened.fixedwidth.three")
    ss01("fixedwidth.four", "fattened.fixedwidth.four")
    ss01("fixedwidth.four.alt", "fattened.fixedwidth.four.alt")
    ss01("fixedwidth.five", "fattened.fixedwidth.five")
    ss01("fixedwidth.six", "fattened.fixedwidth.six")
    ss01("fixedwidth.seven", "fattened.fixedwidth.seven")
    ss01("fixedwidth.seven.alt", "fattened.fixedwidth.seven.alt")
    ss01("fixedwidth.eight", "fattened.fixedwidth.eight")
    ss01("fixedwidth.nine", "fattened.fixedwidth.nine")


# Character variant 'cv47' selects alternative glyphs for digits
# 'four' and 'seven'.

def add_feature_cv47(font):
    font.addLookup("cv47", "gsub_single", 0,
                   (("cv47",
                     (("DFLT",
                       ("dflt")), )), ))
    font.addLookupSubtable("cv47", "alternativesFourSeven")

    def cv47(glyph, variant):
        font[glyph].addPosSub("alternativesFourSeven", variant)

    cv47("four", "four.alt")
    cv47("seven", "seven.alt")
    cv47("fixedwidth.four", "fixedwidth.four.alt")
    cv47("fixedwidth.seven", "fixedwidth.seven.alt")
    cv47("fattened.four", "fattened.four.alt")
    cv47("fattened.seven", "fattened.seven.alt")
    cv47("fattened.fixedwidth.four", "fattened.fixedwidth.four.alt")
    cv47("fattened.fixedwidth.seven", "fattened.fixedwidth.seven.alt")


# Tabular figures feature 'tnum'.

def add_feature_tnum(font):
    font.addLookup("tnum", "gsub_single", 0,
                   (("tnum",
                     (("DFLT",
                       ("dflt")), )), ))
    font.addLookupSubtable("tnum", "fixedWidth")

    def tnum(glyph, variant):
        font[glyph].addPosSub("fixedWidth", variant)

    tnum("zero", "fixedwidth.zero")
    tnum("one", "fixedwidth.one")
    tnum("two", "fixedwidth.two")
    tnum("three", "fixedwidth.three")
    tnum("four", "fixedwidth.four")
    tnum("four.alt", "fixedwidth.four.alt")
    tnum("five", "fixedwidth.five")
    tnum("six", "fixedwidth.six")
    tnum("seven", "fixedwidth.seven")
    tnum("seven.alt", "fixedwidth.seven.alt")
    tnum("eight", "fixedwidth.eight")
    tnum("nine", "fixedwidth.nine")

    tnum("fattened.zero", "fattened.fixedwidth.zero")
    tnum("fattened.one", "fattened.fixedwidth.one")
    tnum("fattened.two", "fattened.fixedwidth.two")
    tnum("fattened.three", "fattened.fixedwidth.three")
    tnum("fattened.four", "fattened.fixedwidth.four")
    tnum("fattened.four.alt", "fattened.fixedwidth.four.alt")
    tnum("fattened.five", "fattened.fixedwidth.five")
    tnum("fattened.six", "fattened.fixedwidth.six")
    tnum("fattened.seven", "fattened.fixedwidth.seven")
    tnum("fattened.seven.alt", "fattened.fixedwidth.seven.alt")
    tnum("fattened.eight", "fattened.fixedwidth.eight")
    tnum("fattened.nine", "fattened.fixedwidth.nine")


# Optional input ligatures for specially slashed figured bass digits,
# off by default.

def add_feature_dlig(font):
    # We need a dummy backslash character.
    font.createChar(ord("\\"), "backslash")
    # Without the next code line, FontForge considers the 'backslash'
    # glyph as unused and refuses to produce ligatures with it.
    font["backslash"].width = 0;

    font.addLookup("dlig", "gsub_ligature", 0,
                   (("dlig",
                     (("DFLT",
                       ("dflt")), )), ))
    font.addLookupSubtable("dlig", "slashed")

    def dlig(glyph, left, right):
        font[glyph].addPosSub("slashed", (left, right))

    dlig("figbass.sixstroked", "six", "backslash")
    dlig("figbass.sevenstroked", "seven", "backslash")
    dlig("figbass.sevenstroked", "seven.alt", "backslash")
    dlig("figbass.ninestroked", "nine", "backslash")

    dlig("figbass.twoplus", "two", "plus")
    dlig("figbass.fourplus", "four", "plus")
    dlig("figbass.fourplus", "four.alt", "plus")
    dlig("figbass.fiveplus", "five", "plus")

# eof
