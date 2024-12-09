# -*- coding: utf-8 -*-
# book_docbook.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2020--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>,
# Copyright (C) 2010--2023 Reinhold Kainhofer <reinhold@kainhofer.com>
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


import os

import book_base
import book_snippets


# Recognize special sequences in the input.
#
#   (?P<name>regex) -- Assign result of REGEX to NAME.
#   *?              -- Match non-greedily.
#   (?!...)         -- Match if `...' doesn't match next (without
#                      consuming the string).
#
#   (?m) -- Multiline regex: Make ^ and $ match at each line.
#   (?s) -- Make the dot match all characters including newline.
#   (?x) -- Ignore whitespace in patterns.
#
# See book_base.BookOutputFormat for possible keys.
Docbook_snippet_res = {
    'lilypond':
    r'''(?smx)
          < (?P<inline> (inline)? ) mediaobject> \s*
          (?P<match>
            <textobject .*? > \s*
            <programlisting \s+
              language = ["'] lilypond ["'] .*?
              ( role = ["'] (?P<options> .*? ) ["'] )? \s*
            >
            (?P<code> .*? ) \s*
            </programlisting \s* > \s*
            </textobject \s* > \s*
          )
          </ (inline)? mediaobject>''',

    'lilypond_block':
    r'''(?smx)
          < (?P<inline> (inline)? ) mediaobject> \s*
          (?P<match>
            <textobject .*? > \s*
            <programlisting \s+
              language = ["'] lilypond ["'] .*?
              ( role = ["'] (?P<options> .*? ) ["'] )? \s*
            >
            (?P<code> .*? ) \s*
            </programlisting \s* > \s*
            </textobject \s* > \s*
          )
          </ (inline)? mediaobject>''',

    'lilypond_file':
    r'''(?smx)
          < (?P<inline> (inline)? ) mediaobject> \s*
          (?P<match>
            <imageobject .*? > \s*
            <imagedata \s+
              fileref = ["'] (?P<filename> .*? \.ly ) ["'] \s*
              ( role = ["'] (?P<options> .*? ) ["'] )? \s*
            ( /> | > \s* </imagedata> ) \s*
            </imageobject> \s*
          )
          </ (inline)? mediaobject>''',

    'multiline_comment':
    r'''(?smx)
          (?P<match>
            \s* (?! @c \s+)
            (?P<code> <!-- \s .*? !--> ) \s
          )''',
}


Docbook_output = {
    book_snippets.FILTER: r'''<mediaobject>
  <textobject>
    <programlisting language="lilypond"
                    role="%(options)s">
%(code)s
    </programlisting>
  </textobject>
</mediaobject>''',

    # TODO: this looks wrong: in PDF, we should use per-system output
    book_snippets.OUTPUT: r'''<imageobject role="latex">
  <imagedata fileref="%(base)s.pdf" format="PDF"/>
</imageobject>
<imageobject role="html">
  <imagedata fileref="%(base)s.png" format="PNG"/>
</imageobject>''',

    book_snippets.PRINTFILENAME: r'''<textobject>
  <simpara>
    <ulink url="%(base)s%(ext)s">
      <filename>
        %(filename)s
      </filename>
    </ulink>
  </simpara>
</textobject>''',

    book_snippets.VERBATIM: r'''<textobject>
  <programlisting language="lilypond"
                  role="processed">%(verb)s</programlisting>
</textobject>''',

    book_snippets.VERSION: r'''%(program_version)s''',
}


class BookDocbookOutputFormat (book_base.BookOutputFormat):
    def __init__(self):
        book_base.BookOutputFormat.__init__(self)
        self.format = "docbook"
        self.default_extension = ".xml"
        self.snippet_res = Docbook_snippet_res
        self.output = Docbook_output
        self.handled_extensions = ['.lyxml']
        self.snippet_option_separator = r'\s+'

    def adjust_snippet_command(self, cmd):
        if '-dseparate-page-formats' not in cmd:
            cmd += ' -dseparate-page-formats=pdf '
        if '-dtall-page-formats' not in cmd:
            cmd += ' -dtall-page-formats=eps,pdf,png '
        return cmd

    def snippet_output(self, basename, snippet):
        s = ''
        rep = snippet.get_replacements()
        for image in snippet.get_images():
            rep['image'] = image
            (rep['base'], rep['ext']) = os.path.splitext(image)
            s += self.output[book_snippets.OUTPUT] % rep
            s += self.output_print_filename(basename, snippet)
            s += '\n'
        if book_snippets.VERBATIM in snippet.option_dict:
            rep['verb'] = book_base.verbatim_html(snippet.verb_ly())
            s += self.output[book_snippets.VERBATIM] % rep
            s += '\n'

        return s


book_base.register_format(BookDocbookOutputFormat())
