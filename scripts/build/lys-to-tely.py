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

import sys
import os
import argparse
import re
import glob
import textwrap

from pathlib import Path
from argparse import HelpFormatter


include_snippets = '@lysnippets'
menu_snippets = '@lymenu'


def find_file(name):
    if os.path.isabs(name):
        if os.path.isfile(name):
            return name
        else:
            return None

    for d in options.include_path:
        p = os.path.join(d, name)
        if os.path.isfile(p):
            return p
    return None


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
        epilog=f"If a lilypond-book options file exists for an input file"
               f" (stripping off the file name's suffix and appending"
               f" `.lybook`) that gets eventually included with"
               f" `@musicxmlfile` or `@lilypondfile`, use its contents as"
               f" additional fragment options (separated by whitespace) for"
               f" this input file.\n"
               f"\n"
               f"If a file gets included with `@lilypondfile`, a `@node`"
               f" line is generated right before it, with its argument"
               f" enclosed in macro `@lynode` (which must be defined in the"
               f" template). If this file contains a `doctitle` field in its"
               f" `\\header` block, it uses the `doctitle` value as the"
               f" `@node` argument. If it doesn't contain such a field (or"
               f" if the file can't be read), the file name is used as the"
               f" argument for `@node`. Additionally, the Texinfo template"
               f" should contain a marker `{menu_snippets}` to indicate"
               f" where to insert the corresponding `@menu` block. In this"
               f" block, each entry is enclosed with macro `@lyentry` (which"
               f" must be also defined in the template).")

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
    '-I', '--include',
    type=str,
    action='append',
    dest='include_path',
    default=['.'],
    metavar='DIR',
    help='append %(metavar)s to include path for input files'
         ' (default: %(default)s)')
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
    f = find_file(options.input_filenames)
    if f is not None:
        files = open(f, encoding='utf-8').read().split()

template_default = rf'''\input texinfo

@c This file was autogenerated
@c     from: %s
@c     by:   %s

@settitle {options.title}

@documentencoding UTF-8
@afourpaper

@macro lynode{{TEXT}}
@node \TEXT\
@end macro

@macro lyentry{{TEXT}}
\TEXT\
@end macro

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

{menu_snippets}

{include_snippets}

@bye
''' % ("\n@c           ".join(files), sys.argv[0])

template = template_default
if options.template:
    f = find_file(options.template)
    if f is not None:
        template = open(f, 'r', encoding='utf-8').read()


doctitle_re = re.compile(r'''(?sx)
    \\header \s* {
      .*? \s
      doctitle \s* = \s* " ( (?: [^"\\] | \\. )* ) "
    \s .*? }''')


def get_node_name(ly_file):
    node_name = os.path.basename(ly_file)

    f = find_file(ly_file)
    if f is not None:
        ly = open(f, encoding='utf-8').read()
        m = doctitle_re.search(ly)
        if m:
            # Replace newlines with spaces as a safety measure.
            node_name = ' '.join(m.group(1).split())

    # ',' and ':' must be protected since they can cause trouble with
    # `@node` and/or within a `@menu` block.  We also undo '\"'.
    return (node_name.replace(',', '@comma{}')
                     .replace(':', '@asis{:}')
                     .replace(r'\"', '"'))


html_file_re = re.compile(r'.*\.i?html?$')
info_file_re = re.compile(r'.*\.info$')
pdf_file_re = re.compile(r'.*\.i?pdf$')
tex_file_re = re.compile(r'.*\.i?(la)?tex$')
texi_file_re = re.compile(r'.*\.i?te(ly|xi|xinfo)$')
xml_file_re = re.compile(r'.*\.i?(xm|mx)l$')


def name2line(n):
    menu_entry = None

    file_name = Path(n)
    fragment_options_file = file_name.with_suffix('.lybook')

    fragment_options_string = ''
    f = find_file(fragment_options_file)
    if f is not None:
        fragment_options = open(f, encoding='utf-8').read().split()
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
        node_name = get_node_name(n)
        s = r"""
@ifhtml
@html
<a name="%s"></a>
@end html
@end ifhtml

@lynode{%s}
@lilypondfile[%s]{%s}
""" % (os.path.basename(n),
       node_name,
       options.fragment_options + fragment_options_string,
       options.prefix + n)

        menu_entry = '* @lyentry{%s}::' % node_name

    return (s, menu_entry)


if files:
    snippet_list = []
    menu_entry_list = []

    for f in files:
        (snippet, menu_entry) = name2line(f)
        snippet_list.append(snippet)
        if menu_entry:
            menu_entry_list.append(menu_entry)

    if menu_entry_list:
        menu = '@menu\n'
        menu += '\n'.join(menu_entry_list)
        menu += '\n@end menu\n'
    else:
        menu = ''

    snippets = '\n'.join(snippet_list)

    s = (template.replace(menu_snippets, menu, 1)
                 .replace(include_snippets, snippets, 1))
    h = open(options.output, "w", encoding="utf8")
    h.write(s)
    h.close()
else:
    # not Unix philosophy, but hey, at least we notice when
    # we don't distribute any .ly files.
    sys.stderr.write(
        "No files specified. Doing nothing. Use -h to display usage.\n")
