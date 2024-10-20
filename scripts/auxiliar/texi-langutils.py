#!/usr/bin/env python
# texi-langutils.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2006--2023 Jan Nieuwenhuizen <janneke@gnu.org>
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
import re
import argparse
import os

from argparse import HelpFormatter
from operator import attrgetter


# `@untranslated` should be defined as a macro in the Texinfo source.
node_blurb_default = '''\
@untranslated
'''
intro_blurb_default = '''\
%(input_texinfo)s@c -*- coding: utf-8; mode: texinfo%(doclang)s -*-
@c This file is part of %(topfile)s
@ignore
    Translation of GIT committish: %(head_committish)s
    When revising a translation, copy the HEAD committish of the
    version that you are working on.  See TRANSLATION for details.
@end ignore
'''
end_blurb = '''\
@c -- SKELETON FILE --
'''

texinfo_with_menus_re = re.compile(
    r'''(?mx)
            ^ ([*]) [ ]+ ([^:\n]+) :: .*? $      # menu entries
          |
            ^ @
            (include
             | menu
             | end [ ]+ menu
             | node
             | (?: unnumbered | appendix) (?: (?: sub){0,2} sec)?
             | top
             | chapter
             | (?: sub){0,2} section
             | (?: major | chap | (?: sub){0,2}) heading
            )
            [ ]* (.*?) $                       # structuring commands
          |
            @ (rglos) { (.+?) }                # glossary references
    ''')
texinfo_re = re.compile(
    r'''(?mx)
            ^ @
            (include
             | node
             | (?: unnumbered | appendix) (?: (?: sub){0,2} sec)?
             | top
             | chapter
             | (?: sub){0,2} section
             | (?: major | chap | (?: sub){0,2}) heading
            )
            [ ]* (.+?) $                       # structuring commands
          |
            @ (rglos) { (.+?) }                # glossary references
    ''')
ly_string_re = re.compile(
    r'''(?x)
            ^ ([a-zA-Z]+) [\t ]* =             # variable definition starts
          |
            %+ [\t ]* (.*) $                   # comments
          |
            \\ (?: new | context ) \s+
            [a-zA-Z]*?
            (?: Staff (?: Group)?
             | Voice
             | FiguredBass
             | FretBoards
             | Names
             | Devnull) \s+
            = \s+
            "? ([a-zA-Z]+) "? \s+              # context identifiers
    ''')
lsr_verbatim_ly_re = re.compile(r'% begin verbatim$')
texinfo_verbatim_ly_re = re.compile(r'^@lilypond\[.*?verbatim')


def read_pipe(command):
    print(command)
    pipe = os.popen(command)
    output = pipe.read()
    if pipe.close():
        print("pipe failed: %(command)s" % locals())
    return output


# Find a file using `texi2any`'s algorithm.
def find_file(name, prior_directory='.'):
    # First, try to open the file relative to `prior_directory`.
    p = os.path.join(prior_directory, name)
    if os.path.isfile(p):
        return p

    # Then search the file in the include path.
    for d in options.include_path:
        p = os.path.join(d, name)
        if os.path.isfile(p):
            return p

    return None


def process_texi(texifilename,
                 i_blurb, n_blurb,
                 write_skeleton, topfile,
                 output_file=None, scan_ly=False, inclusion_level=0):
    try:
        f = open(texifilename, 'r', encoding='utf-8')
        texifile = f.read()
        f.close()
        printedfilename = texifilename.replace('../', '')
        includes = []

        # process ly var names and comments
        if output_file and (scan_ly or texifilename.endswith('.ly')):
            lines = texifile.splitlines()
            i = 0
            in_verb_ly_block = False
            if texifilename.endswith('.ly'):
                verbatim_ly_re = lsr_verbatim_ly_re
            else:
                verbatim_ly_re = texinfo_verbatim_ly_re
            for i in range(len(lines)):
                if verbatim_ly_re.search(lines[i]):
                    in_verb_ly_block = True
                elif lines[i].startswith('@end lilypond'):
                    in_verb_ly_block = False
                elif in_verb_ly_block:
                    line_id = '# ' + printedfilename + ':' + str(i + 1)
                    for (var, comment, context_id) \
                            in ly_string_re.findall(lines[i]):
                        if var:
                            output_file.write(
                                line_id +
                                ' (variable)\n_(r"' + var + '")\n')
                        elif comment:
                            comment = comment.replace('"', '\\"')
                            output_file.write(
                                line_id +
                                ' (comment)\n_(r"' + comment + '")\n')
                        elif context_id:
                            output_file.write(
                                line_id +
                                ' (context id)\n_(r"' + context_id + '")\n')

        # process Texinfo node names and section titles
        if write_skeleton:
            p = os.path.join(options.output_dir, texifilename)
            os.makedirs(os.path.dirname(p), exist_ok=True)
            g = open(p, 'w', encoding='utf-8')

            if inclusion_level == 0:
                input_texinfo = r'\input texinfo '
            else:
                input_texinfo = ''

            subst = globals()
            subst.update(locals())
            g.write(i_blurb % subst)

            tutu = texinfo_with_menus_re.findall(texifile)
            node_just_defined = ''
            for item in tutu:
                if item[0] == '*':
                    g.write('* ' + item[1] + '::\n')
                elif output_file and item[4] == 'rglos':
                    output_file.write(
                        '_(r"' + item[5] + '") ' +
                        '# @rglos in ' + printedfilename + '\n')
                elif item[2] == 'menu':
                    g.write('@menu\n')
                elif item[2] == 'end menu':
                    g.write('@end menu\n\n')
                elif item[2] == 'documentlanguage':
                    g.write('@documentlanguage ' + options.language + '\n')
                else:
                    space = ' '
                    if item[3].startswith('{') or not item[3].strip():
                        space = ''
                    g.write('@' + item[2] + space + item[3] + '\n')
                    if node_just_defined:
                        g.write('@translationof ' + node_just_defined + '\n')
                        g.write(n_blurb)
                        node_just_defined = ''
                        if options.head_only and inclusion_level == 1:
                            break
                    elif item[2] == 'include':
                        includes.append(item[3])
                    else:
                        if output_file:
                            output_file.write(
                                '# @' + item[2] +
                                ' in ' + printedfilename +
                                '\n_(r"' + item[3].strip() + '")\n')
                        if item[2] == 'node':
                            node_just_defined = item[3].strip()
            if not options.head_only:
                g.write(end_blurb)
            g.close()

        elif output_file and scan_ly:
            toto = texinfo_re.findall(texifile)
            for item in toto:
                if item[0] == 'include':
                    includes.append(item[1])
                elif item[2] == 'rglos':
                    output_file.write(
                        '# @rglos in ' + printedfilename +
                        '\n_(r"' + item[3] + '")\n')
                else:
                    stripped_item = item[1].strip().replace('\\', r'\\')
                    output_file.write(
                        '# @' + item[0] + ' in ' + printedfilename +
                        '\n_(r"' + stripped_item + '")\n')

        if (options.process_includes
                and (not options.head_only or inclusion_level < 1)):
            dir = os.path.dirname(texifilename)
            for item in includes:
                file_name = item.strip()
                file = find_file(file_name, dir)
                if file is None:
                    print('cannot find include file %s, skipping' % file_name)
                else:
                    process_texi(file,
                                 i_blurb, n_blurb,
                                 write_skeleton, topfile,
                                 output_file, scan_ly, inclusion_level + 1)
    except IOError as xxx_todo_changeme:
        (errno, strerror) = xxx_todo_changeme.args
        sys.stderr.write("I/O error(%s): %s: %s\n" %
                         (errno, texifilename, strerror))


class SortingHelpFormatter(HelpFormatter):
    def add_arguments(self, actions):
        actions = sorted(actions, key=attrgetter('option_strings'))
        super(SortingHelpFormatter, self).add_arguments(actions)


p = argparse.ArgumentParser(
        description='Translation tool for Texinfo files.',
        formatter_class=SortingHelpFormatter,
        usage='%(prog)s [OPTION]... [FILE]...')

p.add_argument('-b', '--node-blurb',
               type=str,
               default=node_blurb_default,
               metavar='BLURB',
               help='change blurb written at each node to BLURB')
p.add_argument('--gettext',
               action='store_true',
               help='generate node list file from Texinfo source')
p.add_argument('--head-only',
               action='store_true',
               help='only write first node in included Texinfo skeletons')
p.add_argument('-i', '--intro-blurb',
               type=str,
               default=intro_blurb_default,
               metavar='BLURB',
               help='change blurb written at beginning of each file to BLURB')
p.add_argument('-I', '--include',
               action='append',
               default=['.'],
               dest='include_path',
               metavar='DIR',
               help='append DIR to include path (default: %(default)s)')
p.add_argument('-l', '--language',
               type=str,
               default='',
               metavar='ISOLANG',
               help='set document language to ISOLANG')
p.add_argument('-n', '--process-includes',
               action='store_false',
               help='do not process @include commands in Texinfo source')
p.add_argument('-o', '--output-dir',
               type=str,
               metavar='DIR',
               help='use DIR as output directory')
p.add_argument('-p', '--output-po',
               type=str,
               default='doc.pot',
               metavar='NAME',
               help='set PO output file name to NAME (default: %(default)s)')
p.add_argument('--skeleton',
               action='store_true',
               help='extract node tree files from Texinfo source')

p.add_argument('texi_files',
               nargs='*',
               metavar='FILEs',
               help='Texinfo source files')

options = p.parse_args()

head_committish = read_pipe('git rev-parse HEAD')

if options.intro_blurb != '':
    options.intro_blurb += '\n\n'
if options.node_blurb != '':
    options.node_blurb = '\n' + options.node_blurb + '\n\n'
if options.language:
    doclang = '; documentlanguage: ' + options.language
if options.gettext:
    node_list_filename_default = 'node_list'

    node_list_filename = os.path.join(options.output_dir,
                                      node_list_filename_default)
    os.makedirs(os.path.dirname(node_list_filename), exist_ok=True)

    node_list = open(node_list_filename, 'w', encoding='utf-8')
    node_list.write('# -*- coding: utf-8 -*-\n')
    for texi_file in options.texi_files:
        file = find_file(texi_file)
        if file is None:
            print('cannot find input file %s, skipping' % texi_file)
        else:
            # Ugly: scan ly comments and variable names only in English doco
            is_english_doc = (
                True
                and 'Documentation/ca/' not in texi_file
                and 'Documentation/cs/' not in texi_file
                and 'Documentation/de/' not in texi_file
                and 'Documentation/es/' not in texi_file
                and 'Documentation/fr/' not in texi_file
                and 'Documentation/hu/' not in texi_file
                and 'Documentation/ja/' not in texi_file
                and 'Documentation/it/' not in texi_file
                and 'Documentation/nl/' not in texi_file
                and 'Documentation/po/' not in texi_file
                and 'Documentation/pt/' not in texi_file
                and 'Documentation/zh/' not in texi_file
            )
            process_texi(texi_file,
                         options.intro_blurb, options.node_blurb,
                         options.skeleton, os.path.basename(texi_file),
                         output_file=node_list, scan_ly=is_english_doc)
    for word in ('Up:', 'Next:', 'Previous:',
                 'Appendix ', 'Footnotes', 'Table of Contents'):
        node_list.write('_(r"' + word + '")\n')
    node_list.close()
    p = os.path.join(options.output_dir, options.output_po)
    os.system('xgettext --keyword=_doc -c -L Python --no-location -o ' +
              p + ' ' + node_list_filename)
else:
    for texi_file in options.texi_files:
        file = find_file(texi_file)
        if file is None:
            print('cannot find input file %s, skipping' % texi_file)
        else:
            process_texi(file,
                         options.intro_blurb, options.node_blurb,
                         options.skeleton, os.path.basename(texi_file))
