#!/usr/bin/env python
# update-snippets.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2007--2022 John Mandereau <john.mandereau@gmail.com>>
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


# USAGE:  update-snippets.py REFERENCE-DIR TARGET-DIR FILES
#
# update ly snippets in TARGET-DIR/FILES with snippets from REFERENCE-DIR/FILES
#
# More precisely, each existing FILE in TARGET-DIR is matched to the FILE in
# REFERENCE-DIR (it the latter does not exist, a warning is given).
#
# Shell wildcards expansion is performed on FILES.
# This script currently supports Texinfo format.
# Ly snippets preceded with a line containing '@c KEEP LY' in TARGET-DIR/FILES
# will not be updated.
# An error occurs if REFERENCE-DIR/FILE and TARGET-DIR/FILE do not have the
# same snippets count.

import sys
import os
import glob
import re
from functools import reduce

print("update-snippets.py")

comment_re = re.compile(
    r'(?<!@)(@c(?:omment)? .*?\n|^@ignore\n.*?\n@end ignore\n)', re.M | re.S)
snippet_re = re.compile(
    r'^(@lilypond(?:file)?(?:\[.*?\])?\s*\{.+?\}|@lilypond(?:\[.*?\])?(?:.|\n)+?@end lilypond)', re.M)


def snippet_split(l):
    r = []
    for s in [s for s in l if s]:
        if s.startswith('@c ') or s.startswith('@ignore\n') or s.startswith('@comment '):
            r.append(s)
        else:
            r += [t for t in snippet_re.split(s) if t]
    return r


def count_snippet(l):
    k = 0
    for s in l:
        if s.startswith('@lilypond'):
            k += 1
    return k


def find_next_snippet(l, k):
    while not l[k].startswith('@lilypond'):
        k += 1
    return k


exit_code = 0


def update_exit_code(code):
    global exit_code
    exit_code = max(code, exit_code)


ref_dir, target_dir = sys.argv[1:3]
file_patterns = sys.argv[3:]

total_snippet_count = 0
changed_snippets_count = 0

for pattern in file_patterns:
    files = glob.glob(os.path.join(target_dir, pattern))
    for file in files:
        ref_file = os.path.join(ref_dir, os.path.basename(file))
        if not os.path.isfile(ref_file):
            sys.stderr.write(
                "Warning: %s: no such file.\nReference file for %s not found.\n" % (ref_file, file))
            continue
        f = open(file, 'r', encoding='utf-8')
        target_source = comment_re.split(f.read())
        f.close()
        if reduce(lambda x, y: x or y, ['-- SKELETON FILE --' in s for s in target_source]):
            sys.stderr.write("Skipping skeleton file %s\n" % file)
            continue
        g = open(ref_file, 'r', encoding='utf-8')
        ref_source = comment_re.split(g.read())
        target_source = snippet_split(target_source)
        ref_source = snippet_split(ref_source)
        if '' in target_source or '' in ref_source:
            raise RuntimeError(
                "target_source and ref_source must not be empty")
        snippet_count = count_snippet(target_source)
        if not snippet_count == count_snippet(ref_source):
            update_exit_code(1)
            sys.stderr.write("Error: %s and %s have different snippet counts.\n\
Update translation by at least adding a @lilypond block where necessary, then rerun this script.\n" % (ref_file, file))
            continue
        total_snippet_count += snippet_count
        c = 0
        k = -1
        for j in range(len(target_source)):
            if target_source[j].startswith('@lilypond'):
                k = find_next_snippet(ref_source, k+1)
                if j > 0 and (not target_source[j-1].startswith('@c KEEP LY')) and target_source[j] != ref_source[k]:
                    target_source[j] = ref_source[k]
                    c += 1
                    changed_snippets_count += 1
        f = open(file, 'w', encoding='utf-8')
        f.write(''.join(target_source))
        sys.stderr.write('%s: %d/%d snippets updated\n' %
                         (file, c, snippet_count))

sys.stderr.write('\nTotal: %d snippets, %d updated snippets.\n' %
                 (total_snippet_count, changed_snippets_count))
sys.exit(exit_code)
