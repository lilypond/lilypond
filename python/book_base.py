# book_base.py
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

import os
import re
import sys

import book_snippets as BookSnippet
import book_snippets
import lilylib as ly

progress = ly.progress
warning = ly.warning
error = ly.error

########################################################################
# Helper functions
########################################################################


def find_file(name, include_path, raise_error=True):
    for i in include_path:
        full = os.path.join(i, name)
        if os.path.exists(full):
            return full

    if raise_error:
        error(_("file not found: %s") % name + '\n')
        sys.exit(1)
    return ''


def verbatim_html(s):
    return re.sub('>', '&gt;',
                  re.sub('<', '&lt;',
                         re.sub('&', '&amp;', s)))


########################################################################
# Option handling
########################################################################

# TODO: Definitions just once in all files!!!
LINE_WIDTH = 'line-width'

# TODO: Implement the intertext snippet option:
#         'intertext': r',?\s*intertext=\".*?\"',

default_snippet_opts = {
    'alt': "[image of music]",
    # A4 paper defaults.  The default line width is set in function
    # `compose_ly` (file `book_snippets.py`).
    book_snippets.PAPER_HEIGHT: r'845.047\pt',
    book_snippets.PAPER_WIDTH: r'597.508\pt',
}


########################################################################
# format handling
########################################################################

all_formats = []


def register_format(fmt):
    all_formats.append(fmt)


########################################################################
# Snippet handling
########################################################################

# Use this for sorting the keys of the defined snippet types (the dict
# is unsorted, so we need to return sorted keys to ensure processing
# in a pre-defined order)
# Containing blocks must be first, see find_toplevel_snippets.
snippet_type_order = [
    'multiline_comment',
    'verbatim',
    'verb',
    'lilypond_block',
    'singleline_comment',
    'lilypond_file',
    'include',
    'lilypond',
    'lilypondversion',
]

########################################################################
# Base class for all output formats
########################################################################


class BookOutputFormat:
    def __init__(self):
        self.format = None
        self.default_extension = None

        # snippet_type string => regex string

        # Possible keys are:
        #     'multiline_comment', 'verbatim', 'lilypond_block', 'singleline_comment',
        #     'lilypond_file', 'include', 'lilypond', 'lilypondversion'
        #     'musicxml_file',

        self.snippet_res = {}
        self.output = {}
        self.handled_extensions = []
        self.image_formats = "ps,png"
        self.global_options = {}
        self.document_language = ''
        self.default_snippet_options = default_snippet_opts
        self.snippet_option_separator = r"\s*,\s*"

    def supported_snippet_types(self):
        """List of snippet types (strings)"""
        # Sort according to snippet_type_order, unknown keys come last
        keys = list(self.snippet_res.keys())
        # First the entries in snippet_type_order in that order (if present)
        # then all entries not in snippet_type_order in given order
        res = [x for x in snippet_type_order if x in keys] + \
            [x for x in keys if x not in snippet_type_order]
        return res

    def snippet_regexp(self, snippet_type):
        """return regex string for snippet type"""
        return self.snippet_res.get(snippet_type, None)

    def can_handle_format(self, format):
        return format == self.format

    def can_handle_extension(self, extension):
        return extension in self.handled_extensions

    def add_options(self, option_parser):
        pass

    def process_options(self, global_options):
        pass

    def process_options_pdfnotdefault(self, global_options):
        # prevent PDF from being switched on by default.
        formats = ['eps']
        if global_options.create_pdf:
            global_options.process_cmd += ' -dinclude-eps-fonts -dgs-load-fonts '
            formats.append('pdf')
            if global_options.latex_program == 'latex':
                global_options.latex_program = 'pdflatex'
        if '-dseparate-page-formats' not in global_options.process_cmd:
            global_options.process_cmd += ' -dseparate-page-formats=%s ' % (','.join(formats))

    def snippet_class(self, type):
        return BookSnippet.snippet_type_to_class.get(type, BookSnippet.Snippet)

    def get_document_language(self, source):
        return ''

    def init_default_snippet_options(self, source):
        self.document_language = self.get_document_language(source)
        self.default_snippet_options.update(self.get_paper_geometry(source))

    def get_paper_geometry(self, source):
        return {}

    def split_snippet_options(self, option_string):
        if option_string:
            return re.split(self.snippet_option_separator, option_string)
        return []

    def input_fullname(self, input_filename):
        return find_file(input_filename, self.global_options.include_path)

    def adjust_snippet_command(self, cmd):
        return cmd

    def process_chunks(self, chunks):
        return chunks

    def snippet_output(self, basename, snippet):
        warning(_("Output function not implemented"))
        return ''

    def output_simple(self, type, snippet):
        return self.output.get(type, '') % snippet.get_replacements()

    def output_simple_replacements(self, type, variables):
        return self.output.get(type, '') % variables

    def output_print_filename(self, basename, snippet,
                              option=book_snippets.PRINTFILENAME):
        s = ''
        rep = snippet.get_replacements()
        if option in snippet.option_dict:
            rep['base'] = basename
            rep['filename'] = os.path.basename(snippet.filename)
            rep['ext'] = snippet.ext
            s = self.output[option] % rep

        return s

    def required_files(self, snippet, base, full, required_files):
        return []

    def required_files_png(self, snippet, base, full, required_files):
        # UGH - junk global_options
        res = []
        if (base + '.eps' in required_files and not snippet.global_options.skip_png_check):
            page_count = BookSnippet.ps_page_count(full + '.eps')
            if page_count <= 1:
                res.append(base + '.png')
            else:
                for page in range(1, page_count + 1):
                    res.append(base + '-page%d.png' % page)
        return res


def find_linestarts(s):
    """Return a list of indices indicating the first char of a line."""
    nls = [0]
    start = 0
    end = len(s)
    while True:
        i = s.find('\n', start)
        if i < 0:
            break

        i = i + 1
        nls.append(i)
        start = i

    nls.append(len(s))
    return nls


def find_toplevel_snippets(input_string, formatter, global_options):
    res = {}
    types = formatter.supported_snippet_types()
    for t in types:
        res[t] = re.compile(formatter.snippet_regexp(t))

    snippets = []
    index = 0
    found = dict([(t, None) for t in types])

    line_starts = find_linestarts(input_string)
    line_start_idx = 0
    # We want to search for multiple regexes, without searching
    # the string multiple times for one regex.
    # Hence, we use earlier results to limit the string portion
    # where we search.
    # Since every part of the string is traversed at most once for
    # every type of snippet, this is linear.
    while True:
        first = None
        endex = 1 << 30
        for type in types:
            if not found[type] or found[type][0] < index:
                found[type] = None

                m = res[type].search(input_string, index, endex)
                if not m:
                    continue

                klass = formatter.snippet_class(type)

                start = m.start('match')
                line_number = line_start_idx
                while line_starts[line_number] < start:
                    line_number += 1

                line_number += 1
                snip = klass(type, m, formatter, line_number, global_options)

                found[type] = (start, snip)

            if (found[type]
                and (not first
                     or found[type][0] < found[first][0])):
                first = type

                # FIXME.

                # Limiting the search space is a cute
                # idea, but this *requires* to search
                # for possible containing blocks
                # first, at least as long as we do not
                # search for the start of blocks, but
                # always/directly for the entire
                # @block ... @end block.

                endex = found[first][0]

        if not first:
            snippets.append(BookSnippet.Substring(
                input_string, index, len(input_string), line_start_idx))
            break

        while start > line_starts[line_start_idx+1]:
            line_start_idx += 1

        (start, snip) = found[first]
        snippets.append(BookSnippet.Substring(
            input_string, index, start, line_start_idx + 1))
        snippets.append(snip)
        found[first] = None
        index = start + len(snip.match.group('match'))

    return snippets
