# lys-to-tely.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2002--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>,
# Copyright (C) 2001--2023 Jan Nieuwenhuizen <janneke@gnu.org>
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

'''
TODO:

 * Add @nodes, split at sections?
'''


import sys
import os
import argparse
import re
import glob
import textwrap

from pathlib import Path
from argparse import HelpFormatter


include_snippets = '@lysnippets'


# A wrapper around `textwrap.wrap()` that keeps newlines in the input string
# intact.  Taken from https://stackoverflow.com/questions/3853722.
def wrap_paragraphs(text, width, indent):
    lines = []
    for i in text.splitlines():
        paragraph_lines = textwrap.wrap(i, width,
                                        initial_indent=indent,
                                        subsequent_indent=indent)
        # `textwrap.wrap()` returns an empty list when passed an empty
        # string (which happens when there are two consecutive line breaks
        # in the input string).  This would lead to those line breaks being
        # collapsed into a single line break, effectively removing empty
        # lines from the input.  Thus, we add an empty line in that case.
        lines.extend(paragraph_lines or [''])
    return lines


class CustomHelpFormatter(HelpFormatter):
    # Sort options alphabetically, ignoring case ...
    def add_arguments(self, actions):
        # ... but don't sort '--zzz' before '-a'.
        actions = \
          sorted(actions,
                 key=lambda x:
                         x.option_strings[0].replace('--', '-').lower()
                             if x.option_strings else None)
        super(CustomHelpFormatter, self).add_arguments(actions)

    # Preserve newlines in `help` arguments.
    def _split_lines(self, text, width):
        return wrap_paragraphs(text, width, '')

    # Preserve newlines in `description` and `epilog` arguments.
    def _fill_text(self, text, width, indent):
        return '\n'.join(wrap_paragraphs(text, width, indent))


p = argparse.ArgumentParser(
        formatter_class=CustomHelpFormatter,
        usage='%(prog)s [OPTION]... [FILE]...',
        description="Construct a 'tely' doc file from LilyPond files.",
        epilog="If a lilypond-book options file exists for an input file"
               " (stripping off the file name's suffix and appending"
               " `.lybook`) that gets eventually included with"
               " `@musicxmlfile` or `@lilypondfile`, use its contents as"
               " additional fragment options (separated by whitespace) for"
               " this input file.")

p.add_argument(
    '-f', '--fragment-options',
    type=str,
    default='printfilename,texidoc',
    metavar='OPTIONS',
    help="use %(metavar)s as lilypond-book fragment options"
         " (default: '%(default)s')")
p.add_argument(
    '-o', '--output',
    type=str,
    default='ly-doc.tely',
    metavar='FILE',
    help="write 'tely' doc to %(metavar)s (default: '%(default)s')")
p.add_argument(
    '--prefix',
    type=str,
    default='',
    metavar='PREFIX',
    help='prefix file names with %(metavar)s')
p.add_argument(
    '-i', '--input-filenames',
    type=str,
    default='',
    metavar='FILE',
    help='read list of files from %(metavar)s instead of stdin')
p.add_argument(
    '-g', '--glob-input',
    type=str,
    metavar='GLOB',
    help='a string to be passed as `glob.glob(%(metavar)s)` for getting a'
         ' list of files, ignoring stdin; setting this option overrides'
         ' `--input-filenames`')
p.add_argument(
    '-t', '--title',
    type=str,
    default='Ly Doc',
    metavar='TITLE',
    help="set 'tely' doc title to %(metavar)s (default: '%(default)s')")
p.add_argument(
    '-a', '--author',
    type=str,
    default='The LilyPond development team',
    metavar='AUTHOR',
    help="set 'tely' doc author to %(metavar)s (default: '%(default)s')")
p.add_argument(
    '--template',
    type=str,
    metavar='FILE',
    help=f'use %(metavar)s as a custom Texinfo template file instead of the'
         f' default template; %(metavar)s should contain a marker'
         f' `{include_snippets}` to indicate where to insert the LilyPond'
         f' source files.\n'
         f'\n'
         f'If a custom template is used, options `--title` and `--author`'
         f' are ignored')

p.add_argument(
    'files',
    nargs='*',
    metavar='FILEs',
    help='LilyPond source files. Depending on the file extensions they are'
         ' handled as follows.\n'
         '`.[i]html`, `.info`, `.[i]pdf`, `.[i][la]tex`: use HTML link.\n'
         '`.[i]tely`, `.[i]texi`, `.[i]texinfo`: use `@include`.\n'
         '`.[i]xml`, `.[i]mxl`: use `@musicxmlfile`.\n'
         'Use `@lilypondfile` for everything else.')

options = p.parse_args()

files = options.files
if options.glob_input:
    files = glob.glob(options.glob_input)
elif options.input_filenames:
    files = open(options.input_filenames, encoding='utf-8').read().split()

template_default = rf'''\input texinfo

@c This file was autogenerated
@c     from: %s
@c     by:   %s

@settitle {options.title}

@documentencoding UTF-8
@afourpaper

@finalout @c we do not want black boxes.

@titlepage
@title LilyPond
@subtitle The music typesetter
@titlefont{{{options.title}}}
@author {options.author}
@end titlepage

@ifnottex
@node Top
@top {options.title}
@end ifnottex

{include_snippets}

@bye
''' % (", ".join(files), sys.argv[0])

if options.template:
    template = open(options.template, 'r', encoding='utf-8').read()
else:
    template = template_default


html_file_re = re.compile(r'.*\.i?html?$')
info_file_re = re.compile(r'.*\.info$')
pdf_file_re = re.compile(r'.*\.i?pdf$')
tex_file_re = re.compile(r'.*\.i?(la)?tex$')
texi_file_re = re.compile(r'.*\.i?te(ly|xi|xinfo)$')
xml_file_re = re.compile(r'.*\.i?(xm|mx)l$')


def name2line(n):
    file_name = Path(n)
    fragment_options_file = file_name.with_suffix('.lybook')

    fragment_options_string = ''
    if os.path.isfile(fragment_options_file):
        fragment_options = open(fragment_options_file, encoding='utf-8').read().split()
        if fragment_options:
            fragment_options_string = ',' + ','.join(fragment_options)

    if texi_file_re.match(n):
        # We have a Texinfo file; simply include it.
        s = r"@include %s" % os.path.basename(n)

    elif (html_file_re.match(n) or info_file_re.match(n)
          or pdf_file_re.match(n) or tex_file_re.match(n)):
        # Use HTML links for formats that can't be further processed.
        s = r"""
@ifhtml
@html
<a href="%s">%s</a>
<br/>
@end html
@end ifhtml
""" % (os.path.basename(n), os.path.basename(n))

    elif xml_file_re.match(n):
        # Assume it's a MusicXML file -> convert, create image, etc.
        s = r"""
@ifhtml
@html
<a name="%s"></a>
@end html
@end ifhtml

@musicxmlfile[%s]{%s}
""" % (os.path.basename(n),
       options.fragment_options + fragment_options_string,
       options.prefix + n)

    else:
        # Assume it's a LilyPond file -> create image, etc.
        s = r"""
@ifhtml
@html
<a name="%s"></a>
@end html
@end ifhtml

@lilypondfile[%s]{%s}
""" % (os.path.basename(n),
       options.fragment_options + fragment_options_string,
       options.prefix + n)
    return s


if files:
    s = "\n".join(map(name2line, files))
    s = template.replace(include_snippets, s, 1)
    h = open(options.output, "w", encoding="utf8")
    h.write(s)
    h.close()
else:
    # not Unix philosophy, but hey, at least we notice when
    # we don't distribute any .ly files.
    sys.stderr.write(
        "No files specified. Doing nothing. Use -h to display usage.\n")
