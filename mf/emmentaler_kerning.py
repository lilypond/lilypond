# emmentaler_kerning.py
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
OpenType kerning for LilyPond's Emmentaler Font
"""


import fontforge


# It is no longer trivial to import kerning data from
# `feta-alphabetXXX.tfm` because we have kerning values for glyphs
# that don't get assigned to input character code points by default.
# Additionally, we use OpenType font features.  For these reasons we
# add kerning to the Emmentaler fonts while post-processing the
# Metafont subfonts.
#
# * `feta-numbers.mf` originally contained kerning data for time
#   signature digits.
#
#   ```
#   [number_design_size = design_size / 2]
#
#   space# := number_design_size / 2;
#
#   ligtable "3":
#     "3" kern 0.1 space#,
#     "0" kern 0.1 space#;
#
#   ligtable "2":
#     "7" kern 0.15 space#;
#   ```
#
# * `feta-dynamics.mf` originally contained kerning data for letters
#   used in dynamics.
#
#   ```
#   [pt# = 1]
#   [dynamic_design_size = 14 design_size / 20]
#
#   ex# := (dynamic_design_size / 2.4) * pt#;
#   horizontal_space# := .66 ex#;
#
#   fet_beginchar ("space", "space");
#     set_char_box (0, horizontal_space#, 0, ex#);
#   fet_endchar;
#
#   ligtable "m":
#     "p" kern 0.2 ex#,
#     "f" kern -0.1 ex#;
#
#   ligtable "n":
#     "p" kern 0.2 ex#,
#     "f" kern -0.1 ex#;
#
#   ligtable "f":
#     "f" kern -0.13 ex#;
#
#   ligtable "r":
#     "f" kern 0.1 ex#;
#   ```
#
# Based on this, we use the width of the 'space' character (which can
# be found in the intermediate PFB file and is thus accessible by
# FontForge) to derive kern values.
#
# We change the names of `horizontal_space#`, `space#`, and `ex#` to
# the more meaningful names `SW` (space width), `NKU` (numbers kerning
# unit), and `DKU` (dynamics kerning unit), respectively.  We express
# `NKU` and `DKU` as multiples of `SW`.
#
# ```
# ex# = (dynamic_design_size / 2.4) * pt#
#     = (14 design_size / 20) / 2.4
#     = design_size * 14/20 * 10/24
#     = design_size * 7/2 * 1/12
#     = 7/24 design_size
#
# SW = .66 ex# = 33/50 DKU
# DKU = 50/33 SW
#
# design_size = 24/7 * ex#
#             = 24/7 * 50/33 SW
#             = 8/7 * 50/11 SW
#             = 400/77 SW
#
# space# = number_design_size / 2
#        = design_size / 4
# NKU = 100/77 SW
# ```


# The kerning should always be applied regardless of script and
# language; we thus use the 'DFLT' script and 'dflt' (dummy) language
# tags.
def add_feature_kern(font):
    # TODO: Check whether kerning should also depend on design size.
    SW = font["space"].width
    DKU = 50.0/33.0 * SW
    NKU = 100.0/77.0 * SW

    # It is easier to keep track of kerning by our own than to use
    # the generic FontForge API to get data from the 'GPOS' table.
    kerning = {}

    font.addLookup("kern", "gpos_pair", 0,
                   (("kern",
                     (("DFLT",
                       ("dflt")), )), ))
    font.addLookupSubtable("kern", "kern")

    # The left side bearing factors are taken from `feta-numbers.mf`.
    lsb_factors = {
        "zero": 1.0,
        "one": 1.2,
        "two": 1.0,
        "three": 1.0,
        "four": 1.0,
        "four.alt": 1.0, # same value as 'four'
        "five": 0.7,
        "six": 1.0,
        "seven": 1.2,
        "seven.alt": 1.2, # same value as 'seven'
        "eight": 1.0,
        "nine": 1.0,

        # A zero lsb factor indicates zero lsb and rsb values.  We
        # also use this to distinguish between digits and non-digits
        # in this dictionary.
        "hyphen": 0.0,
        "plus": 0.0,
        "period": 0.0,
        "comma": 0.0
    }

    lsb_factors_keys = sorted(lsb_factors.keys())

    def kern(left, right, val):
        v = int(round(val))
        kerning[(left, right)] = v
        font[left].addPosSub("kern", right, 0, 0, v, 0, 0, 0, 0, 0)

    # First, the standard kerning pairs for digits and punctuation.
    kern("zero", "zero", 0.15 * NKU)
    kern("zero", "one", 0.1 * NKU)
    kern("zero", "five", 0.05 * NKU)
    kern("zero", "six", 0.1 * NKU)
    kern("zero", "seven", 0.1 * NKU)
    kern("zero", "eight", 0.05 * NKU)
    kern("zero", "nine", 0.1 * NKU)
    kern("zero", "hyphen", 0.1 * NKU)
    kern("zero", "plus", 0.1 * NKU)

    kern("one", "zero", -0.1 * NKU)
    kern("one", "four", -0.1 * NKU)
    kern("one", "six", -0.1 * NKU)
    kern("one", "period", 0.1 * NKU)
    kern("one", "comma", 0.1 * NKU)

    kern("two", "one", 0.1 * NKU)
    kern("two", "three", 0.1 * NKU)
    kern("two", "six", 0.1 * NKU)
    kern("two", "seven", 0.15 * NKU)
    kern("two", "eight", 0.05 * NKU)
    kern("two", "nine", 0.1 * NKU)
    kern("two", "period", 0.05 * NKU)
    kern("two", "comma", 0.05 * NKU)

    kern("three", "one", 0.1 * NKU)
    kern("three", "zero", 0.1 * NKU)
    kern("three", "two", 0.05 * NKU)
    kern("three", "three", 0.1 * NKU)
    kern("three", "five", 0.1 * NKU)
    kern("three", "six", 0.1 * NKU)
    kern("three", "seven", 0.1 * NKU)
    kern("three", "eight", 0.1 * NKU)
    kern("three", "nine", 0.1 * NKU)
    kern("three", "hyphen", 0.1 * NKU)
    kern("three", "period", 0.1 * NKU)
    kern("three", "comma", 0.1 * NKU)

    kern("four", "hyphen", 0.1 * NKU)
    kern("four", "period", 0.1 * NKU)
    kern("four", "comma", 0.1 * NKU)

    kern("five", "zero", 0.15 * NKU)
    kern("five", "one", 0.15 * NKU)
    kern("five", "two", 0.1 * NKU)
    kern("five", "three", 0.15 * NKU)
    kern("five", "four", 0.1 * NKU)
    kern("five", "five", 0.15 * NKU)
    kern("five", "six", 0.15 * NKU)
    kern("five", "seven", 0.15 * NKU)
    kern("five", "eight", 0.15 * NKU)
    kern("five", "nine", 0.15 * NKU)
    kern("five", "plus", 0.1 * NKU)
    kern("five", "hyphen", 0.1 * NKU)
    kern("five", "period", 0.1 * NKU)
    kern("five", "comma", 0.1 * NKU)

    kern("six", "zero", 0.1 * NKU)
    kern("six", "one", 0.05 * NKU)
    kern("six", "two", 0.15 * NKU)
    kern("six", "three", 0.1 * NKU)
    kern("six", "five", 0.1 * NKU)
    kern("six", "six", 0.1 * NKU)
    kern("six", "seven", 0.1 * NKU)
    kern("six", "eight", 0.1 * NKU)
    kern("six", "nine", 0.1 * NKU)
    kern("six", "hyphen", 0.1 * NKU)
    kern("six", "period", 0.1 * NKU)
    kern("six", "comma", 0.1 * NKU)

    kern("seven", "four", -0.2 * NKU)
    kern("seven", "five", 0.1 * NKU)
    kern("seven", "seven", 0.15 * NKU)
    kern("seven", "nine", 0.1 * NKU)
    kern("seven", "hyphen", -0.05 * NKU)
    kern("seven", "period", -0.1 * NKU)
    kern("seven", "comma", -0.1 * NKU)

    kern("eight", "zero", 0.1 * NKU)
    kern("eight", "one", 0.1 * NKU)
    kern("eight", "two", 0.05 * NKU)
    kern("eight", "three", 0.1 * NKU)
    kern("eight", "four", 0.05 * NKU)
    kern("eight", "five", 0.1 * NKU)
    kern("eight", "six", 0.1 * NKU)
    kern("eight", "seven", 0.1 * NKU)
    kern("eight", "eight", 0.1 * NKU)
    kern("eight", "nine", 0.1 * NKU)
    kern("eight", "plus", 0.1 * NKU)
    kern("eight", "hyphen", 0.1 * NKU)
    kern("eight", "period", 0.1 * NKU)
    kern("eight", "comma", 0.1 * NKU)

    kern("nine", "zero", 0.15 * NKU)
    kern("nine", "one", 0.15 * NKU)
    kern("nine", "two", 0.05 * NKU)
    kern("nine", "three", 0.1 * NKU)
    kern("nine", "six", 0.1 * NKU)
    kern("nine", "seven", 0.15 * NKU)
    kern("nine", "eight", 0.05 * NKU)
    kern("nine", "nine", 0.1 * NKU)
    kern("nine", "plus", 0.1 * NKU)
    kern("nine", "hyphen", 0.1 * NKU)

    kern("hyphen", "zero", 0.1 * NKU)
    kern("hyphen", "one", 0.1 * NKU)
    kern("hyphen", "three", 0.1 * NKU)
    kern("hyphen", "four", 0.1 * NKU)
    kern("hyphen", "five", 0.1 * NKU)
    kern("hyphen", "six", 0.1 * NKU)
    kern("hyphen", "seven", 0.1 * NKU)
    kern("hyphen", "eight", 0.1 * NKU)
    kern("hyphen", "nine", 0.1 * NKU)

    kern("plus", "zero", 0.1 * NKU)
    kern("plus", "one", 0.05 * NKU)
    kern("plus", "six", 0.05 * NKU)
    kern("plus", "seven", 0.1 * NKU)

    kern("period", "one", 0.1 * NKU)
    kern("period", "two", 0.15 * NKU)
    kern("period", "three", 0.1 * NKU)
    kern("period", "five", 0.1 * NKU)
    kern("period", "seven", 0.05 * NKU)
    kern("period", "eight", 0.1 * NKU)
    kern("period", "nine", 0.1 * NKU)
    kern("period", "period", 0.15 * NKU)
    kern("period", "comma", 0.15 * NKU)

    kern("comma", "one", 0.1 * NKU)
    kern("comma", "two", 0.15 * NKU)
    kern("comma", "three", 0.1 * NKU)
    kern("comma", "five", 0.1 * NKU)
    kern("comma", "seven", 0.05 * NKU)
    kern("comma", "eight", 0.1 * NKU)
    kern("comma", "nine", 0.1 * NKU)
    kern("comma", "period", 0.15 * NKU)
    kern("comma", "comma", 0.15 * NKU)

    # Kerning for alternative digits forms; they are identical to the
    # standard glyphs.
    for left in lsb_factors_keys:
        for right in lsb_factors_keys:
            if (not (lsb_factors[left] or lsb_factors[right])):
                continue

            if (not (left == "four" or left == "seven"
                     or right == "four" or right == "seven")):
                continue

            if (left, right) in kerning:
                kern_val = kerning[(left, right)]

                left_suffix = ".alt" \
                    if (left == "four" or left == "seven") else ""
                right_suffix = ".alt" \
                    if (right == "four" or right == "seven") else ""

                kern(left + left_suffix, right + right_suffix, kern_val)

    # Compute kern values for 'fattened' digits.  They are the same
    # as for normal digits.
    for left in lsb_factors_keys:
        for right in lsb_factors_keys:
            if (not (lsb_factors[left] or lsb_factors[right])):
                continue

            if (left, right) in kerning:
                kern_val = kerning[(left, right)]

                left_prefix = "fattened." if lsb_factors[left] else ""
                right_prefix = "fattened." if lsb_factors[right] else ""

                kern(left_prefix + left, right_prefix + right, kern_val)

    # For fixed-width digits (both normal and fattened) the goal is to
    # make all such digit pairs have the same horizontal distance as
    # non-fixed-width digits if the 'kern' feature is active.
    #
    #               +---------+
    #               |         |
    #               |  glyph  |
    #               |         |
    #               +---------+
    #
    #       |-------|---------|-------|
    #          lsb     digit     rsb
    #                  width
    #       |-------------------------|
    #         fixed-width digit width

    fixed_width_digit_width = font["four"].width

    def lsb(glyph):
        width = font[glyph].width
        lsb_factor = lsb_factors[glyph]
        if (lsb_factor == 0.0):
            return 0

        return lsb_factor * (fixed_width_digit_width - width) / 2.0

    def rsb(glyph):
        width = font[glyph].width
        lsb_factor = lsb_factors[glyph]
        if (lsb_factor == 0.0):
            return 0

        return fixed_width_digit_width - width - lsb(glyph)

    for left in lsb_factors_keys:
        for right in lsb_factors_keys:
            if (not (lsb_factors[left] or lsb_factors[right])):
                continue

            if (left, right) in kerning:
                kern_val = kerning[(left, right)]
            else:
                kern_val = 0

            correction = rsb(left) + lsb(right)
            figbass_kern = kern_val - correction
            if (figbass_kern == 0.0):
                continue

            left_prefix = "fixedwidth." if lsb_factors[left] else ""
            right_prefix = "fixedwidth." if lsb_factors[right] else ""
            kern(left_prefix + left, right_prefix + right, figbass_kern)

            left_prefix = "fattened.fixedwidth." if lsb_factors[left] else ""
            right_prefix = "fattened.fixedwidth." if lsb_factors[right] else ""
            kern(left_prefix + left, right_prefix + right, figbass_kern)

    # Finally, the kerning for the few letters.
    kern("f", "f", -0.13 * DKU)
    kern("m", "p", 0.2 * DKU)
    kern("m", "f", -0.1 * DKU)
    kern("n", "p", 0.2 * DKU)
    kern("n", "f", -0.1 * DKU)
    kern("r", "f", 0.1 * DKU)
    kern("s", "p", 0.3 * DKU)

# eof
