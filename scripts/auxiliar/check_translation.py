#!/usr/bin/env python

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2007--2022 John Mandereau <john.mandereau@gmail.com>
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

import __main__
import optparse
import os
import sys
import re

import langdefs
import buildlib

verbose = 0
use_colors = False
lang = 'C'
C = lang


def dir_lang(file, lang, lang_dir_index):
    path_components = file.split('/')
    path_components[lang_dir_index] = lang
    return os.path.join(*path_components)


# ugh, this is complicated; where has the good old 'git rev-parse' gone?
vc_revision_parse = 'git log -1 --pretty=format:%%H %s'


def do_file(file_name, lang_codes):
    if verbose:
        sys.stderr.write('%s...\n' % file_name)
    split_file_name = file_name.split('/')
    d1, d2 = split_file_name[0:2]
    if d1 in lang_codes:
        check_lang = d1
        lang_dir_index = 0
    elif d2 in lang_codes:
        check_lang = d2
        lang_dir_index = 1
    else:
        check_lang = lang
    if check_lang == C:
        raise Exception('cannot determine language for ' + file_name)
    else:
        if os.path.splitext(file_name)[1] == '.texidoc':
            original = file_name.replace(os.path.join(
                check_lang, 'texidocs'), 'snippets', 1)
            original = original.replace('.texidoc', '.ly', 1)
        else:
            original = dir_lang(file_name, 'en', lang_dir_index)
        translated_contents = open(file_name, encoding='utf-8').read()

        # experimental feature
        if not touch_committishes in (current_revision, 'HEAD'):
            (changes_in_original, error) = \
                buildlib.check_translated_doc(original,
                                              file_name,
                                              translated_contents,
                                              upper_revision=touch_committishes)
            if not error and not changes_in_original in ('', '\n'):
                translated_contents = \
                    buildlib.revision_re.sub('GIT committish: ' + current_revision,
                                             translated_contents, 1)
                f = open(file_name, 'w', encoding='utf-8').write(translated_contents)
                return

    (diff_string, error) \
        = buildlib.check_translated_doc(original,
                                        file_name,
                                        translated_contents,
                                        color=use_colors and not update_mode)

    if error:
        sys.stderr.write('warning: %s: %s' % (file_name, error))

    if update_mode:
        if error or len(diff_string) >= os.path.getsize(original):
            buildlib.read_pipe(text_editor + ' ' + file_name + ' ' + original)
        elif diff_string:
            diff_file = original + '.diff'
            f = open(diff_file, 'w', encoding='utf-8')
            f.write(diff_string)
            f.close()
            buildlib.read_pipe(text_editor + ' ' + file_name + ' ' + diff_file)
            os.remove(diff_file)
    else:
        sys.stdout.write(diff_string)


def usage():
    sys.stdout.write(r'''
Usage:
check-translation [--language=LANG] [--verbose] [--update] [-t COMMIT] FILE...
''')


def do_options():
    global lang, verbose, update_mode, touch_committishes, use_colors

    p = optparse.OptionParser(usage="check-translation [--language=LANG] [--verbose] [--update] [-t COMMIT] FILE...",
                              description="")
    p.add_option("--language",
                 action='store',
                 default=C,
                 dest="language",
                 metavar='LL',
                 help="assume document language ISO 639 code LL by default")
    p.add_option("--no-color",
                 action='store_false',
                 default=True,
                 dest="color",
                 help="do not print ANSI-colored output")
    p.add_option("--verbose",
                 action='store_true',
                 default=False,
                 dest="verbose",
                 help="print details, including executed shell commands")
    p.add_option('-t',
                 action='store',
                 default='HEAD',
                 dest="touch_committishes",
                 metavar='COMMIT',
                 help='[EXPERIMENTAL] update committishes of all files that were up to \
date at commit COMMIT')
    p.add_option('-u', "--update",
                 action='store_true',
                 default=False,
                 dest='update_mode',
                 help='call $EDITOR to update the translation')

    (options, files) = p.parse_args()
    verbose = options.verbose
    lang = options.language
    use_colors = options.color
    update_mode = options.update_mode
    touch_committishes = options.touch_committishes

    return files


def main():
    global update_mode, text_editor, touch_committishes, current_revision

    files = do_options()
    if 'EDITOR' in os.environ:
        text_editor = os.environ['EDITOR']
    else:
        update_mode = False
    buildlib.verbose = verbose
    (parsed_revision, error) = buildlib.read_pipe(
        vc_revision_parse % touch_committishes)
    if error:
        sys.stderr.write('warning: %s' % error)
    else:
        touch_committishes = parsed_revision.strip()
    current_revision = buildlib.read_pipe(vc_revision_parse % 'HEAD')[0]

    for i in files:
        do_file(i, list(langdefs.LANGDICT.keys()))


if __name__ == '__main__':
    main()
