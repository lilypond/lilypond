# book_texinfo.py
# -*- coding: utf-8 -*-
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2010--2026 Reinhold Kainhofer <reinhold@kainhofer.com>
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
import subprocess
import sys
import tempfile

import book_base
import book_snippets
import lilylib as ly

# See `book_latex.py` for some regex documentation.
#
# All lilypond-book commands except the '@lilypond...@end lilypond' block
# use braces to delimit their argument.  We thus follow the Texinfo syntax
# to allow these commands anywhere in the input file.  To make this work we
# have to take care of not hitting '@lilypond{...}' and friends within a
# comment: we use a negative lookahead to check for '@c' and '@comment'.
# Also ensure that we don't have a single '@' right before the command.
#
# We additionally skip some Texinfo commands to avoid parsing text that
# looks like lilypond-book commands (for example, '@lilypond{...}' within a
# 'verbatim' environment), and to handle included files.  Regarding
# whitespace, we are more lenient than what's described in the Texinfo
# manual, which has been made more strict in recent years.
TexInfo_snippet_res = {
    'include': r'''(?mx)
          ^ [ \t]*
          (?P<match>
            @include
            [ \t]+
            (?P<filename> .*? ) )
          [ \t]* $''',

    'lilypond': r'''(?mx)
          ^
          (?! [^\n]*? (?: @c | @comment ) (?: [ \t] | @ ) )
          (?: @ [^\n] | [^@\n] )*
          (?P<match>
            @lilypond
            \s*
            (?: \[ \s* (?P<options> [^\[\]]*? ) \s* \] )?
            \s*
            { (?P<code>''' + ly.brace_matcher(10) + r''' ) \s* }
          )''',

    # Since this is a block command, the opening bracket for options (if
    # any) must be on the same line as '@lilypond' (contrary to
    # '@lilypond[...]{...}').
    'lilypond_block': r'''(?smx)
          ^ [ \t]*
          (?P<match>
            @lilypond
            [ \t]*
            (?: \[ \s* (?P<options> [^\[\]]*? ) \s* \] )?
            \s+?
            ^ (?P<code> .*? ) \s*
            ^ [ \t]* @end [ \t]+ lilypond )
          [ \t]* $''',

    'lilypond_file': r'''(?mx)
          ^
          (?! [^\n]*? (?: @c | @comment ) (?: [ \t] | @ ) )
          (?: @ [^\n] | [^@\n] )*
          (?P<match>
            @lilypondfile
            \s*
            (?: \[ \s* (?P<options> [^\[\]]*? ) \s* \] )?
            \s*
            { (?P<filename> [^\n}]+ ) }
          )''',

    'multiline_comment': r'''(?smx)
          ^ [ \t]*
          (?P<match>
            (?P<code>
              @ignore
              \s .*?
              ^ [ \t]* @end [ \t]+ ignore
            ) )
          [ \t]* $''',

    'musicxml_file': r'''(?mx)
          ^
          (?! [^\n]*? (?: @c | @comment ) (?: [ \t] | @ ) )
          (?: @ [^\n] | [^@\n] )*
          (?P<match>
            @musicxmlfile
            \s*
            (?: \[ \s* (?P<options> [^\[\]]*? ) \s* \] )?
            \s*
            { (?P<filename> [^\n}]+ ) }
          )''',

    'singleline_comment': r'''(?mx)
          ^
          .*
          (?P<match>
            (?P<code>
              (?: @c | @comment ) (?: [ \t] [^\n]* | ) \n
            )
          )''',

    'verb': r'''(?sx)
          (?P<match>
            (?P<code>
              @verb{ (?P<del>.)
              .*?
              (?P=del) }
            )
          )''',

    # Only a single space is allowed between '@end' and 'verbatim'.
    'verbatim': r'''(?smx)
          ^ [ \t]*
          (?P<match>
            @verbatim \s+?
            ^ (?P<code> .*? )
            ^ [ \t]* @end [ ] verbatim )
          [ \t]* $''',

    'lilypondversion': r'''(?mx)
          [^@]
          (?P<match> @lilypondversion )
          [^a-zA-Z]
          ''',
}


TexInfo_output = {
    book_snippets.FILTER: r'''@lilypond[%(options)s]
%(code)s
@end lilypond''',

    book_snippets.HTMLPRINTFILENAME: '''@ifhtml
@inlineraw{html, <a href="%(base)s%(ext)s">}
@file{%(filename)s}
@inlineraw{html, </a>}
@end ifhtml
''',

    book_snippets.OUTPUT: r'''@iftex
@include %(base)s-systems.texi
@end iftex''',

    book_snippets.OUTPUTIMAGE: r'''@ifinfo
@image{%(info_image_path)s,,,%(alt)s}
@end ifinfo
@html
<a href="%(base)s%(ext2)s">
  <img align="middle"
       border="0"
       src="%(image)s"
       alt="%(alt)s"></a>
@end html
''',

    # '\x7F' (DEL) works like a true TeX comment character; we use it to not
    # emit any additional whitespace while still having readable Texinfo
    # output.
    book_snippets.INLINEOUTPUT:
'''@inlinefmt{info, @image{%(info_image_path)s,,,%(alt)s}}\x7F
@inlineraw{html,
<a href="%(base)s%(ext2)s">
  <img align="middle"
       border="0"
       src="%(image)s"
       alt="%(alt)s"></a>}\x7F
@inlinefmt{tex,@image{%(image_base)s-1}}''',

    # There must be an empty line at the end to ensure that the following
    # images are typeset in vertical mode (and not in inline mode).
    book_snippets.PRINTFILENAME: '''@need 800
@inlineraw{html, <a href="%(base)s%(ext)s">}
@file{%(filename)s}
@inlineraw{html, </a>}

''',

    book_snippets.QUOTE: r'''@quotation
%(str)s
@end quotation''',

    book_snippets.VERBATIM: r'''@verbatim
%(verb)s@end verbatim
''',

    book_snippets.VERSION: r'''%(program_version)s''',
}


texinfo_line_widths = {
    '@afourpaper': '160\\mm',
    '@afourwide': '6.5\\in',
    '@afourlatex': '150\\mm',
    '@smallbook': '5\\in',
    '@letterpaper': '6\\in',
}


###
# Retrieve dimensions from texinfo
TEXINFO_INSPECTION_DOCUMENT = r'''
\input texinfo
@settitle Texinfo width test
%(preamble)s

@message{Global: textwidth=@the@hsize,exampleindent=@the@lispnarrowing}

dummy

@bye
'''


def get_texinfo_width_indent(source, global_options):
    # TODO: Check for end of header command "@c %**end of header"
    #      only use material before that comment ?

    # extract all relevant papter settings from the input:
    pagesize = None
    texinfo_paper_size_regexp = r'''(@(?:afourpaper|afourwide|afourlatex|afivepaper|smallbook|letterpaper))'''
    m = re.search(texinfo_paper_size_regexp, source)
    if m:
        pagesize = m.group(1)

    relevant_settings_regexp = r'''(@(?:fonttextsize|pagesizes|cropmarks|exampleindent).*)\n'''
    m = re.findall(relevant_settings_regexp, source)
    if pagesize:
        m.insert(0, pagesize)
    # all relevant options to insert into the test document:
    preamble = "\n".join(m)

    texinfo_document = TEXINFO_INSPECTION_DOCUMENT % {'preamble': preamble}

    (handle, tmpfile) = tempfile.mkstemp('.texi')
    outfile = os.path.splitext(tmpfile)[0] + '.pdf'

    tmp_handle = open(handle, 'w', encoding='utf-8')
    tmp_handle.write(texinfo_document)
    tmp_handle.close()

    # Work around a texi2pdf bug: if LANG=C is not given, a broken regexp is
    # used to detect relative/absolute paths, so the absolute path is not
    # detected as such and this command fails:
    ly.progress(
        _("Running texi2pdf on file %s to detect default page settings.\n") % tmpfile)

    # execute the command and pipe stdout to the parameter_string:
    cmd = '%s -c -o %s %s' % (
        global_options.texinfo_program, outfile, tmpfile)
    ly.debug_output("Executing: %s\n" % cmd)
    run_env = os.environ.copy()
    run_env['LC_ALL'] = 'C'

    # unknown why this is necessary
    universal_newlines = True
    if sys.platform == 'mingw32':
        universal_newlines = False
        # use os.system to avoid weird sleep() problems on
        # GUB's python 2.4.2 on mingw
        # make file to write to
        output_dir = tempfile.mkdtemp()
        output_filename = os.path.join(output_dir, 'output.txt')
        # call command
        cmd += " > %s" % output_filename
        returncode = os.system(cmd)
        parameter_string = open(output_filename, encoding="utf8").read()
        if returncode != 0:
            ly.warning(_("Unable to auto-detect default settings:\n"))
        # clean up
        os.remove(output_filename)
        os.rmdir(output_dir)
    else:
        proc = subprocess.Popen(cmd,
                                env=run_env,
                                universal_newlines=universal_newlines,
                                shell=True,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        (parameter_string, error_string) = proc.communicate()
        if proc.returncode != 0:
            ly.warning(_("Unable to auto-detect default settings:\n%s")
                       % error_string)
    os.unlink(tmpfile)
    if os.path.exists(outfile):
        os.unlink(outfile)

    # Find textwidth and exampleindent and format it as \\mm or \\in
    # Use defaults if they cannot be extracted
    textwidth = 0
    m = re.search('textwidth=([0-9.]+)pt', parameter_string)
    if m:
        val = float(m.group(1))/72.27
        if pagesize and pagesize.startswith("@afour"):
            textwidth = "%g\\mm" % round(val*25.4, 3)
        else:
            textwidth = "%g\\in" % round(val, 3)
    else:
        textwidth = texinfo_line_widths.get(pagesize, "6\\in")

    exampleindent = 0
    m = re.search('exampleindent=([0-9.]+)pt', parameter_string)
    if m:
        val = float(m.group(1))/72.27
        if pagesize and pagesize.startswith("@afour"):
            exampleindent = "%g\\mm" % round(val*25.4, 3)
        else:
            exampleindent = "%g\\in" % round(val, 3)
    else:
        exampleindent = "0.4\\in"

    retval = {book_snippets.LINE_WIDTH: textwidth,
              book_snippets.EXAMPLEINDENT: exampleindent}
    ly.debug_output("Auto-detected values are: %s\n" % retval)
    return retval


texinfo_lang_re = re.compile('(?m)^@documentlanguage (.*?)( |$)')


class BookTexinfoOutputFormat (book_base.BookOutputFormat):
    def __init__(self):
        book_base.BookOutputFormat.__init__(self)
        self.format = "texinfo"
        self.default_extension = ".texi"
        self.snippet_res = TexInfo_snippet_res
        self.output = TexInfo_output
        self.handled_extensions = ['.itely', '.tely', '.texi', '.texinfo']
        self.snippet_option_separator = r'\s*,\s*'

    def can_handle_format(self, format):
        return (book_base.BookOutputFormat.can_handle_format(self, format) or
                (format in ['texi-html', 'texi']))

    def process_options(self, global_options):
        self.process_options_pdfnotdefault(global_options)

    def get_document_language(self, source):
        m = texinfo_lang_re.search(source)
        if m and not m.group(1).startswith('en'):
            return m.group(1)
        else:
            return ''

    def get_paper_geometry(self, source):
        return get_texinfo_width_indent(source, self.global_options)

    def adjust_snippet_command(self, cmd):
        if '-dseparate-page-formats' not in cmd:
            cmd += ' -dseparate-page-formats=png,pdf '
        if '-dtall-page-formats' not in cmd:
            # TODO: the EPS output here is useless for cairo, but the
            # rest of lilypond-book expects it to be there.
            formats = ['eps']
            if not self.global_options.skip_png_check:
                formats.append('png')

            cmd += ' -dtall-page-formats=%s ' % ','.join(formats)
        return cmd

    def output_info(self, basename, snippet):
        s = ''
        rep = snippet.get_replacements()
        rep['base'] = basename
        rep['filename'] = os.path.basename(snippet.filename)
        rep['ext'] = snippet.ext
        if snippet.ext == '.xml' or snippet.ext == '.mxl':
            rep['ext2'] = '.ly'
        else:
            rep['ext2'] = snippet.ext

        if book_snippets.INLINE not in snippet.option_dict:
            for image in snippet.get_images():
                rep1 = copy.copy(rep)
                rep1['base'] = os.path.splitext(image)[0]
                rep1['image'] = image
                rep1['alt'] = snippet.option_dict[book_snippets.ALT]
                rep1['info_image_path'] = os.path.join(
                    self.global_options.info_images_dir, rep1['base'])
                s += self.output[book_snippets.OUTPUTIMAGE] % rep1

            s += self.output[book_snippets.OUTPUT] % rep
        else:
            images = snippet.get_images()
            if images:
                # Only use the first system for inline images.
                image = images[0]
                rep['image_base'] = os.path.splitext(image)[0]
                rep['image'] = image
                rep['alt'] = snippet.option_dict[book_snippets.ALT]
                rep['info_image_path'] = os.path.join(
                    self.global_options.info_images_dir, rep['image_base'])
                s += self.output[book_snippets.INLINEOUTPUT] % rep
        return s

    def snippet_output(self, basename, snippet):
        def find(fn):
            p = os.path.join(self.global_options.output_dir, fn)
            if os.path.exists(p):
                return p
            return ''

        s = ''
        base = basename
        if book_snippets.DOCTITLE in snippet.option_dict:
            doctitle = base + '.doctitle'
            translated_doctitle = doctitle + self.document_language
            for t in [translated_doctitle,  doctitle]:
                fullpath = find(t)
                if fullpath:
                    doctitle = open(fullpath, 'r', encoding='utf-8').read()
                    doctitle = doctitle.replace(",", "@comma{}")
                    s += '\n@lydoctitle %s\n\n' % doctitle
                    break

        if book_snippets.TEXIDOC in snippet.option_dict:
            texidoc = base + '.texidoc'
            translated_texidoc = texidoc + self.document_language
            for t in [translated_texidoc, texidoc]:
                fullpath = find(t)
                if fullpath:
                    # We need two empty lines to enforce a new paragraph
                    # in case the included file doesn't end with a newline
                    # character.
                    s += '@include %s\n\n\n' % t
                    break

        if book_snippets.PRINTFILENAME in snippet.option_dict:
            s += self.output_print_filename(basename, snippet)
        elif book_snippets.HTMLPRINTFILENAME in snippet.option_dict:
            s += self.output_print_filename(basename, snippet,
                                            book_snippets.HTMLPRINTFILENAME)

        if book_snippets.INLINE not in snippet.option_dict:
            substr = ''
            rep = snippet.get_replacements()
            if book_snippets.VERBATIM in snippet.option_dict:
                ly_code = snippet.verb_ly()
                if self.global_options.highlight:
                    from auxiliar.book_highlight import highlight_ly
                    substr = highlight_ly(ly_code)
                else:
                    rep['verb'] = ly_code
                    # Avoid start of `@verbatim` environment at the bottom
                    # of a page with a single line.
                    if s.count(ly_code) > 1:
                        substr += "@need 800\n"
                    substr += self.output[book_snippets.VERBATIM] % rep
            substr += self.output_info(basename, snippet)
            if book_snippets.QUOTE in snippet.option_dict:
                substr = self.output[book_snippets.QUOTE] % {'str': substr}
            s += '\n' + substr + '\n'
        else:
            s = self.output_info(basename, snippet)

        return s

    def required_files(self, snippet, base, full, required_files):
        return self.required_files_png(snippet, base, full, required_files)


book_base.register_format(BookTexinfoOutputFormat())
