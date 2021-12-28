# makesnippets.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2012--2022  John Mandereau <john.mandereau@gmail.com>
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

'''USAGE: makesnippets.py INPUT_DIR OUTPUT_DIR LANGUAGES DOC_DIR

Read all .ly files from INPUT_DIR, insert translations from .texidoc
files found in DOC_DIR/LANG/texdiocs, and write ther result to OUTPUT_DIR.'''

import glob
import sys
import os.path
import re

(input_dir, output_dir, languages_str, doc_dir) = sys.argv[1:5]

languages = languages_str.split(' ')

texidoc_dirs = [os.path.join(doc_dir, lang, 'texidocs')
                for lang in languages if lang]

begin_header_re = re.compile(r'\\header\s*{', re.M)

for f in glob.glob(os.path.join(input_dir, '*.ly')):
    name = os.path.basename(f)
    s = open(f, 'r', encoding='utf-8').read()
    for path in texidoc_dirs:
        texidoc_translation_path = \
            os.path.join(path, os.path.splitext(name)[0] + '.texidoc')
        if os.path.exists(texidoc_translation_path):
            texidoc_translation = open(
                texidoc_translation_path, 'r', encoding='utf-8').read()
            # Since we want to insert the translations verbatim using a
            # regexp, \\ is understood as ONE escaped backslash. So we have
            # to escape those backslashes once more...
            texidoc_translation = texidoc_translation.replace('\\', '\\\\')
            s = begin_header_re.sub('\\g<0>\n' + texidoc_translation, s, 1)
    dest = os.path.join(output_dir, name)
    open(dest, 'w', encoding='utf-8').write(s)
