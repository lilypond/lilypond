# book_highlight.py
# -*- coding: utf-8 -*-
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2021--2023 Jean Abou Samra <jean@abou-samra.fr>
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
Bridge to Pygments, providing a Texinfo formatter.

This file provides a function to lex code and return Texinfo input
printing it highlighted.  When called as a script, it outputs a CSS
stylesheet.
"""

import io
import os.path
import sys

# Python has the amazing ability to import from ZIP archives!
sys.path.insert(
    0,
    os.path.join(
        os.path.dirname(__file__),
        "vendored/pygments.zip/",
    ),
)

from pygments import lex
from pygments.lexers import LilyPondLexer
from pygments.formatters import HtmlFormatter
from pygments.style import Style
from pygments.token import Token


class CustomStyle(Style):
    """A style specifically designed for the LilyPond HTML documentation."""

    night_blue = "#000088"
    purple = "#6C0168"
    dark_red = "#990000"
    grey = "#586178"
    turquoise = "#007070"
    yellowish = "#7A5505"

    styles = {
        Token.Whitespace: "",
        Token.Text: "",
        Token.Keyword: night_blue,
        Token.Comment: grey,
        Token.String: yellowish,
        Token.String.Escape: dark_red,
        Token.String.Symbol: "noinherit",
        Token.Pitch: "",
        Token.Number: turquoise,  # includes durations
        # A bare 11 is not distinguishable from a number, so we highlight
        # the same.
        Token.ChordModifier: turquoise,
        Token.Name.Lvalue: "",
        Token.Name.BackslashReference: night_blue,
        Token.Name.Builtin.MusicCommand: night_blue,
        Token.Name.Builtin.PaperVariable: purple,
        Token.Name.Builtin.HeaderVariable: purple,
        Token.Name.Builtin.MusicFunction: night_blue,
        Token.Name.Builtin.Clef: "",
        Token.Name.Builtin.Scale: night_blue,
        Token.Name.Builtin.RepeatType: "",
        Token.Name.Builtin.Dynamic: yellowish,
        Token.Name.Builtin.Articulation: yellowish,
        Token.Name.Builtin.SchemeFunction: night_blue,
        Token.Name.Builtin.SchemeBuiltin: "bold",
        Token.Name.Builtin.MarkupCommand: night_blue,
        Token.Name.Builtin.Context: purple,
        Token.Name.Builtin.ContextProperty: purple,
        Token.Name.Builtin.Grob: dark_red,
        Token.Name.Builtin.GrobProperty: dark_red,
        Token.Name.Builtin.Translator: purple,
    }

# A custom black-and-white style designed for the PDF documentation.
pdf_styles = {
    Token: "",
    Token.Whitespace: "",
    Token.Text: "",
    Token.Keyword: "bold",
    Token.Comment: "italic",
    Token.String: "",
    Token.String.Escape: "",
    Token.String.Symbol: "",
    Token.Pitch: "",
    Token.Number: "",
    Token.ChordModifier: "",
    Token.Name.Lvalue: "bold",
    Token.Name.BackslashReference: "bold",
    Token.Name.Builtin.MusicCommand: "bold",
    Token.Name.Builtin.PaperVariable: "bold italic",
    Token.Name.Builtin.HeaderVariable: "bold italic",
    Token.Name.Builtin.MusicFunction: "bold",
    Token.Name.Builtin.Clef: "",
    Token.Name.Builtin.Scale: "bold",
    Token.Name.Builtin.RepeatType: "",
    Token.Name.Builtin.Dynamic: "",
    Token.Name.Builtin.Articulation: "bold",
    Token.Name.Builtin.SchemeFunction: "bold",
    Token.Name.Builtin.SchemeBuiltin: "bold",
    Token.Name.Builtin.MarkupCommand: "bold",
    Token.Name.Builtin.Context: "bold italic",
    Token.Name.Builtin.ContextProperty: "bold italic",
    Token.Name.Builtin.Grob: "italic",
    Token.Name.Builtin.GrobProperty: "italic",
    Token.Name.Builtin.Translator: "bold italic",
}

def highlight_ly(s):
    # Convert to list because we need to reuse it.
    tokens = list(lex(s, LilyPondLexer()))
    out = "@html\n"
    # Produce HTML using Pygment's built-in formatter.
    out += '<pre class="verbatim">'
    html = io.StringIO()
    HtmlFormatter(nowrap=True).format(tokens, html)
    # Escape literal "@" for Texinfo.
    out += html.getvalue().replace("@", "@@")
    out += "</pre>\n"
    out += "@end html\n\n@ifnothtml\n@pygments\n"
    lines = []
    for token_type, value in tokens:
        # Escape literal "@", "{" and "}" because we're in
        # @format.
        value = value.replace("@", "@@").replace("{", "@{").replace("}", "@}")
        while token_type not in pdf_styles:
            token_type = token_type.parent
        style_def = pdf_styles[token_type]
        italic = "italic" in style_def
        bold = "bold" in style_def
        # @t{@b{...}} does not work, so we use @tb from common-macros.texi.
        # Likewise, @tbsl does what @t{@b{@i{...}}} would do if it worked.
        if italic and bold:
            fmt = "@tbsl{%s}"
        elif italic:
            fmt = "@t{@i{%s}}"
        elif bold:
            fmt = "@tb{%s}"
        else:
            fmt = "@t{%s}"
        formatted_parts = []
        for part in value.split("\n"): # newlines don't work inside @t
            if part: # empty argument to @t causes trouble
                formatted_parts.append(fmt % part)
            else:
                formatted_parts.append("")
        out += "\n".join(formatted_parts)
    # No leading newline because our input already contains a
    # trailing newline.
    out += "@endPygments\n@end ifnothtml\n"
    return out

if __name__ == "__main__":
    print("/* Autogenerated by Pygments */")
    print(HtmlFormatter(style=CustomStyle).get_style_defs(".highlight"))
