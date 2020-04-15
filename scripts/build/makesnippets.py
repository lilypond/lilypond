# makesnippets.py

'''USAGE: makesnippets.py INPUT_DIR OUTPUT_DIR DOC_DIR

Read all .ly files from INPUT_DIR, insert translations from .texidoc
files found in DOC_DIR/LANG/texdiocs, and write ther result to OUTPUT_DIR.'''

import codecs
import glob
import sys
import os.path
import re

import langdefs

(input_dir, output_dir, doc_dir) = sys.argv[1:4]

texidoc_dirs = [os.path.join (doc_dir, language_code, 'texidocs')
                for language_code in langdefs.LANGDICT]

begin_header_re = re.compile (r'\\header\s*{', re.M)

for f in glob.glob (os.path.join (input_dir, '*.ly')):
    name = os.path.basename (f)
    s = codecs.open (f, 'r', 'utf-8').read ()
    for path in texidoc_dirs:
        texidoc_translation_path = \
            os.path.join (path, os.path.splitext (name)[0] + '.texidoc')
        if os.path.exists (texidoc_translation_path):
            texidoc_translation = codecs.open (texidoc_translation_path, 'r', 'utf-8').read ()
            # Since we want to insert the translations verbatim using a
            # regexp, \\ is understood as ONE escaped backslash. So we have
            # to escape those backslashes once more...
            texidoc_translation = texidoc_translation.replace ('\\', '\\\\')
            s = begin_header_re.sub ('\\g<0>\n' + texidoc_translation, s, 1)
    dest = os.path.join (output_dir, name)
    codecs.open (dest, 'w', 'utf-8').write (s)
