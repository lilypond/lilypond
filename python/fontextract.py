# fontextract.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

import getopt
import os
import re
import sys

dsr_font_regex = re.compile('%%DocumentSuppliedResources: font (.*)')
begin_font_regex = re.compile('%%BeginFont: (.*)')
end_font_regex = re.compile('%%EndFont')
verbose = 0


def scan_files(files):
    file_of_font_dict = {}
    for f in files:
        if verbose:
            sys.stderr.write('Scanning %s\n' % f)

        header = open(f, 'r', encoding='utf8').read()
        idx = 0

        extract_from_this = []
        while idx < len(header):
            match = dsr_font_regex.search(header[idx:])
            if not match:
                break
            name = match.group(1)
            idx += match.end(1)
            if name in file_of_font_dict:
                continue

            file_of_font_dict[name] = f

    return file_of_font_dict


def get_file_fonts_dict(file_of_font_dict):
    dict = {}
    for (n, f) in list(file_of_font_dict.items()):
        if f not in dict:
            dict[f] = []

        dict[f].append(n)

    return dict


def extract_fonts_from_file(extract_from_this, font_dict, filename):
    if extract_from_this:
        curr_font = []
        curr_font_name = []
        in_font = 0
        for l in open(filename, encoding='utf8').readlines():
            if not in_font and begin_font_regex.match(l):
                in_font = 1
                curr_font_name = begin_font_regex.match(l).group(1)
                curr_font = []
            elif in_font and end_font_regex.match(l):
                in_font = 0

                if curr_font_name in extract_from_this:
                    font_dict[curr_font_name] = ''.join(curr_font)
                    if verbose:
                        sys.stderr.write('Extracted %s\n' % curr_font_name)

                    extract_from_this.remove(curr_font_name)
            elif in_font:
                curr_font.append(l)
            if not extract_from_this:
                break

        if extract_from_this:
            sys.stderr.write("Failed to extract %s from %s\n"
                             % (', '.join(extract_from_this), filename))


def write_extracted_fonts(output_file_name, font_dict):
    if verbose:
        sys.stderr.write('Writing fonts to %s\n' % output_file_name)
    output = open(output_file_name, 'w', encoding='utf8')
    output.write('''%!PS-Adobe-3.0
%%VMusage: 0 0
%%Creator: lilypond-extract-fonts
''')

    for x in list(font_dict.keys()):
        output.write('%%%%DocumentSuppliedResources: font %s\n' % x)

    output.write('''%%EndComments\n''')

    for (k, v) in list(font_dict.items()):
        output.write('\n%%%%BeginFont: %s\n' % k)
        output.write(v)
        output.write('\n%%EndFont')


def extract_fonts(output_file_name, input_files):
    d = scan_files(input_files)
    ff = get_file_fonts_dict(d)

    font_dict = {}
    for (file, fonts) in list(ff.items()):
        extract_fonts_from_file(fonts, font_dict, file)

    write_extracted_fonts(output_file_name, font_dict)


if __name__ == '__main__':
    extract_fonts('fonts.ps', sys.argv[1:])
