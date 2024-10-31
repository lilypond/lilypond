#!@PYTHON@
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2008--2023 John Mandereau <john.mandereau@gmail.com>
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


import subprocess
import re
import sys

verbose = False


def read_pipe(command, capture=True):
    if capture:
        child = subprocess.Popen(command,
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE,
                                 shell=True)
    else:
        child = subprocess.Popen(command, shell=True)
    (output, error) = child.communicate()
    code = str(child.wait())
    if capture:
        if not child.stdout or child.stdout.close():
            print("pipe failed: %(command)s" % locals())
        (output, error) = (output.decode('utf-8'), error.decode('utf-8'))
    if code != '0':
        error = code + ' ' + error
    return (output, error)


# Renamed files map to ensure continuity of file history
# Map of new_name: old_name
# This is for handling 69f0ec47 ("Docs: reorganize documentation
# directory structure"), which removed the user/ dir and the topdocs/NEWS.tely file
renames_map = {
    'usage.tely': 'user/lilypond-program.tely',
    'notation.tely': 'user/lilypond.tely',
    'learning.tely': 'user/lilypond-learning.tely',
    'changes.tely': 'topdocs/NEWS.tely',
}

# FIXME: Hardcoded file names!?
manuals_subdirectories_re = \
    re.compile(
        '(usage|automated-engraving|changes|essay|extending|web|learning|notation)/')


def get_old_name(file_path):
    for new_path in renames_map:
        if file_path.endswith(new_path):
            old_file_path = file_path.replace(new_path,
                                              renames_map[new_path])
            break
    else:
        if file_path.endswith('macros.itexi'):
            old_file_path = file_path.replace('macros.itexi',
                                              'user/macros.itexi')
        elif file_path.endswith('.itely'):
            old_file_path = manuals_subdirectories_re.sub('user/',
                                                          file_path)
        elif 'snippets/' in file_path:
            old_file_path = file_path.replace('snippets/',
                                              '../input/lsr/')
        else:
            return file_path
    return old_file_path


def file_exists_at(revision, name):
    cmd = "git show %s:Documentation/%s" % (revision, name)
    if verbose:
        sys.stderr.write('running: %s\n' % cmd)

    child = subprocess.run(cmd,
                           stdout=subprocess.DEVNULL,
                           stderr=subprocess.DEVNULL,
                           shell=True)
    return not child.returncode


revision_re = re.compile(r'GIT [Cc]ommittish:\s+([a-f0-9]+)')

no_committish_fatal_error = """error: %s: no 'GIT committish: <hash>' found.
Please check the whole file against the original in English, then
fill in HEAD committish in the header.
"""

def check_translated_doc(original, translated_file, translated_contents,
                         color=False, upper_revision='HEAD'):
    """Returns the diff of the original relative to the last translation"""
    m = revision_re.search(translated_contents)
    if not m:
        sys.stderr.write(no_committish_fatal_error % translated_file)
        sys.exit(1)
    revision = m.group(1)
    if revision == '0':
        return '', 0

    if color:
        color_flag = '--color --color-words'
    else:
        color_flag = '--no-color'

    current_name = original

    if original.startswith("en/") and not file_exists_at(revision, original):
        # this is to handle the rename in 82d72b7 ("Merge branch doc-build")
        original = original[3:]
    if not file_exists_at(revision, original):
        original = get_old_name(original)

    c = 'git diff -M %(color_flag)s %(revision)s:Documentation/%(original)s \
%(upper_revision)s:Documentation/%(current_name)s' % vars()
    if verbose:
        sys.stderr.write('running: %s\n' % c)
    return read_pipe(c)
