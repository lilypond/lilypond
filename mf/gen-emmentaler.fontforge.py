# gen-emmentaler.fontforge.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2020--2022  Han-Wen Nienhuys <hanwen@lilypond.org>
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

import getopt
import os
import re
import sys

import fontforge

from emmentaler_codes import add_code_points
from emmentaler_features import add_feature_dlig
from emmentaler_features import add_feature_ss01
from emmentaler_features import add_feature_cv47
from emmentaler_features import add_feature_tnum
from emmentaler_kerning import add_feature_kern


(options, files) = \
    getopt.getopt(sys.argv[1:],
                  '',
                  ['in=', 'out=', 'version='])

design_size = 0
version = "dev"
indir = ""
output = ""
for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '--in':
        indir = a
    elif o == '--out':
        output = a
    elif o == '--version':
        version = a
    else:
        print(o)
        raise getopt.error

m = re.search(r"([0-9]*)\.otf", output)
assert m, repr(output)

design_size = int(m.group(1))

font = fontforge.font()
font.familyname = "Emmentaler-%d" % design_size
font.fontname = font.familyname
font.fullname = "Emmentaler-%d" % design_size
font.copyright = """This font is distributed under the GNU General Public License.
As a special exception, if you create a document which uses
this font, and embed this font or unaltered portions of this
font into the document, this font does not by itself cause the
resulting document to be covered by the GNU General Public License.
"""
font.version = version


# Merge all Feta and Parmesan subfonts into a single font.

subfonts = []

for fn in ["feta%(design_size)d.pfb",
           "feta-alphabet%(design_size)d.pfb",
           "feta-flags%(design_size)d.pfb",
           "feta-noteheads%(design_size)d.pfb",
           "parmesan%(design_size)d.pfb",
           "parmesan-noteheads%(design_size)d.pfb"]:
    name = fn % vars()
    font.mergeFonts(os.path.join(indir, name))

    name, _ = os.path.splitext(name)
    subfonts.append(name)


# Assign Unicode values to some glyphs.

add_code_points(font)


# Set code points arbitrarily to Unicode's PUA (Private Use Area) for
# all glyphs that are still unassigned, i.e., all glyphs that have a
# glyph name not part of the Adobe Glyph List (AGL).
#
# This will change to more meaningful, permanent assignments as soon
# as we add support for the Standard Music Font Layout (SMuFL).

i = 0
for glyph in font.glyphs():
    if glyph.unicode < 0:
        glyph.unicode = i + 0xE000
        i += 1


# Include OpenType features, in reverse lookup order.

# GSUB
add_feature_ss01(font)
add_feature_cv47(font)
add_feature_tnum(font)
add_feature_dlig(font)
# GPOS
add_feature_kern(font)


# Include (private) SFNT tables needed by LilyPond.

subfonts_str = ' '.join(subfonts)

lisp = b""
for sub in subfonts:
    lisp += open(os.path.join(indir, sub) + ".lisp", "rb").read()

font.setTableData("LILF", subfonts_str.encode("ascii"))
font.setTableData("LILC", lisp)
font.setTableData("LILY", open(os.path.join(
    indir, "feta%(design_size)d.global-lisp" % vars()), "rb").read())


# Generate font in OTF, SVG, and WOFF formats.

font.generate(output)
base, ext = os.path.splitext(output)
font.generate(base + ".svg")
font.generate(base + ".woff")
