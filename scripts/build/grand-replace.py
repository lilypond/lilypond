# grand-replace.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2009--2022 Jan Nieuwenhuizen <janneke@gnu.org>
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

import datetime
import fnmatch
import os
import re
import subprocess

ignored_files = [
    # files maintained outside of LilyPond
    'config/*',
    'Documentation/en/fdl.itexi',
    'Documentation/en/gpl.itexi',
    'scripts/build/help2man.pl',
    'mf/mf2pt1.mp',
    'scripts/build/mf2pt1.pl',
    'tex/texinfo*.tex',
    'tex/txi-*.tex',
    # files maintained by the translation team
    'po/*.po',
    # historical files
    'Documentation/misc/CHANGES*',
    'Documentation/misc/ChangeLog*',
]


def main():
    all_files = subprocess.run(['git', 'ls-files'],
                               capture_output=True, check=True,
                               encoding='utf-8').stdout.splitlines()
    files = [file for file in all_files
             if not any(fnmatch.fnmatch(file, pat) for pat in ignored_files)]
    year = datetime.datetime.now().year
    last_year = year - 1
    copyright_part = r'(Copyright|\(c\)|\(C\)|@copyright\{\}[^\d-]*)'
    for filename in files:
        with open(filename, encoding='utf-8') as file:
            try:
                content = file.read()
            except UnicodeDecodeError:
                continue # ignore binary files
        content = re.sub(copyright_part + rf'{last_year}(?!-)',
                         rf'\1{last_year}--{year}',
                         content)
        content = re.sub(copyright_part + r'(\d{4})--\d{4}',
                         rf'\1\2--{year}',
                         content)
        with open(filename, 'w', encoding='utf-8') as file:
            file.write(content)

if __name__ == '__main__':
    main()
