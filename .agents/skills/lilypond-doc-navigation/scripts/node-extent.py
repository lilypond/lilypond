#!/usr/bin/env python3

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2026 Daniel Eble <nine.fierce.ballads@gmail.com>
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

import argparse
import re

# https://www.gnu.org/software/texinfo/manual/texinfo/html_node/Structuring-Command-Types.html
HEADING_LEVEL = {
    'chapter': 2,
    'section': 3,
    'subsection': 4,
    'subsubsection': 5,

    'top': 1,
    'unnumbered': 2,
    'unnumberedsec': 3,
    'unnumberedsubsec': 4,
    'unnumberedsubsubsec': 5,

    'appendix': 2,
    'appendixsec': 3,
    'appendixsubsec': 4,
    'appendixsubsubsec': 5,
}

def search_file(the_file, node_name):
    # example requiring escaping: "Debugging C++ code"
    node_re = re.compile(r'^@node ' + re.escape(node_name) + '(,|$)')
    heading_re = re.compile(r'^@([a-z]+) ')

    line_no = 0

    # Find the @node line
    node_line_no = None
    while line := the_file.readline():
        line_no += 1
        if node_re.match(line):
            node_line_no = line_no
            break
    if node_line_no is None:
        return

    # Get the node's heading level from the command on the next line
    heading_line = the_file.readline()
    if not heading_line:
        return
    line_no += 1
    matched = heading_re.match(heading_line)
    if not matched:
        return
    heading_level = HEADING_LEVEL.get(matched.group(1), 0)
    if heading_level <= 0:
        return

    # Continue to the next heading at the same level or above
    end_line_no = None
    while line := the_file.readline():
        line_no += 1
        matched = heading_re.match(line)
        if matched:
            lev = HEADING_LEVEL.get(matched.group(1), 0)
            if 0 < lev <= heading_level:
                end_line_no = line_no - 2 # end before @node
                break

    if end_line_no is None: # passage continues to end of file
        end_line_no = line_no

    print(f'{the_file.name}:{node_line_no}-{end_line_no}')

def main():
    p = argparse.ArgumentParser(description='''Given a Texinfo node name, find
    the extent of the passage anchored to it. The passage is recognized by
    heading level (chapter, section, etc.) and may span multiple nodes. The
    output format is <file_name>:<start_line>-<end_line>.''')
    p.add_argument('node', help='node name (exact)')
    p.add_argument('files', nargs='+', metavar='file', help='file to search')
    options = p.parse_args()
    for file_name in options.files:
        with open(file_name) as f:
            search_file(f, options.node)

if __name__ == '__main__':
    main()
