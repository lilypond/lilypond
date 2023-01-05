# book_html.py
# -*- coding: utf-8 -*-
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

import copy
import os
import re

import book_base
import book_snippets

# See `book_latex.py` for some regex documentation.

HTML_snippet_res = {
    'lilypond':
    r'''(?smx)
          (?P<match>
            <lilypond
            ( \s+ (?P<options> [^:<>]*? ) )?
            \s* : \s*
            (?P<code> .*? )
            \s* />
          )''',

    'lilypond_block':
    r'''(?smx)
          (?P<match>
            <lilypond \s*
            (?P<options> [^:<>]*? )
            \s* >
            (?P<code> .*? ) \s*
            </lilypond \s* >
          )''',

    'lilypond_file':
    r'''(?smx)
          (?P<match>
            <lilypondfile \s*
            (?P<options> [^:<>]*? )
            \s* >
            \s* (?P<filename> .*? ) \s*
            </lilypondfile \s* >
          )''',

    # This is problematic, since `-->` can occur in valid LilyPond code,
    # for example, `c4--->`.
    'multiline_comment':
    r'''(?sx)
          (?P<match>
            \s*
            (?! @c \s+ )
            (?P<code> <!-- .*? -->)
          )''',

    'musicxml_file':
    r'''(?mx)
          (?P<match>
          <musicxmlfile\s*(?P<options>.*?)\s*>
          \s*(?P<filename>.*?)\s*
          </musicxmlfile\s*>)''',

    'lilypondversion':
    r'''(?mx)(?P<match><lilypondversion\s*/>)''',
}


HTML_output = {
    book_snippets.FILTER: r'''<lilypond %(options)s>
%(code)s
</lilypond>
''',

    book_snippets.AFTER: r'''</a>''',

    book_snippets.BEFORE: r'''<a href="%(base)s%(ext)s">''',

    book_snippets.OUTPUT: r'''
  <img align="middle"
       border="0"
       src="%(image)s"
       alt="%(alt)s">''',

    book_snippets.PRINTFILENAME: '<p><tt><a href="%(base)s%(ext)s">%(filename)s</a></tt></p>',

    book_snippets.QUOTE: r'''<blockquote>%(str)s</blockquote>''',

    book_snippets.PARA: r'''<p>%(str)s</p>''',

    book_snippets.VERBATIM: r'''<pre>%(verb)s</pre>
''',

    book_snippets.VERSION: r'''%(program_version)s''',
}


class BookHTMLOutputFormat (book_base.BookOutputFormat):
    def __init__(self):
        book_base.BookOutputFormat.__init__(self)
        self.format = "html"
        self.default_extension = ".html"
        self.snippet_res = HTML_snippet_res
        self.output = HTML_output
        self.handled_extensions = ['.html', '.xml', '.htmly']
        self.snippet_option_separator = r'\s+'

    def split_snippet_options(self, option_string):
        if option_string:
            options = re.findall(r'''[-\w\.-:]+(?:\s*=\s*(?:"[^"]*"|'[^']*'|\S+))?''',
                                 option_string)
            options = [re.sub(r'''^([^=]+=\s*)(?P<q>["'])(.*)(?P=q)''', r'\g<1>\g<3>', opt)
                       for opt in options]
            return options
        return []

    def adjust_snippet_command(self, cmd):
        if '-dtall-page-formats' not in cmd:
            cmd += ' -dtall-page-formats=eps,png '
        return cmd

    def output_images(self, basename, snippet):
        s = ''
        rep = snippet.get_replacements()
        rep['base'] = basename
        rep['filename'] = os.path.basename(snippet.filename)
        rep['ext'] = snippet.ext

        s += self.output[book_snippets.BEFORE] % rep
        for image in snippet.get_images():
            rep1 = copy.copy(rep)
            rep1['image'] = image
            (rep1['base'], rep1['ext']) = os.path.splitext(image)
            rep1['alt'] = snippet.option_dict[book_snippets.ALT]
            s += self.output[book_snippets.OUTPUT] % rep1
        s += self.output[book_snippets.AFTER] % rep
        return s

    def snippet_output(self, basename, snippet):
        s = ''
        s += self.output_print_filename(basename, snippet)
        rep = snippet.get_replacements()

        substr = ''
        if book_snippets.VERBATIM in snippet.option_dict:
            rep['verb'] = book_base.verbatim_html(snippet.verb_ly())
            substr = self.output[book_snippets.VERBATIM] % rep

        subsubstr = self.output_images(basename, snippet)
        if book_snippets.INLINE not in snippet.option_dict:
            subsubstr = self.output[book_snippets.PARA] % {'str': subsubstr}
        substr += subsubstr

        if book_snippets.QUOTE in snippet.option_dict:
            substr = self.output[book_snippets.QUOTE] % {'str': substr}
        s += substr

        return s

    def required_files(self, snippet, base, full, required_files):
        return self.required_files_png(snippet, base, full, required_files)


book_base.register_format(BookHTMLOutputFormat())
