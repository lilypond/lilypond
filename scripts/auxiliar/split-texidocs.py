import os
import sys
import re
import glob

source_files = glob.glob (os.path.join ('input', 'texidocs', '*.texidoc'))
dest_path = os.path.join ('Documentation', '%s', 'texidocs', '%s')

s = 'Translation of GIT [Cc]ommittish'
texidoc_chunk_re = re.compile (r'^(?:%+\s*' + s + \
    r'.+)?\s*(?:texidoc|doctitle)([a-zA-Z]{2,4})\s+=(?:.|\n)*?(?=%+\s*' + \
    s + r'|$(?!.|\n))', re.M)

for file_name in source_files:
    base_name = os.path.basename (file_name)
    contents = open (file_name).read ()
    for match in texidoc_chunk_re.finditer (contents):
        language_code = match.group (1)
        print language_code
        open (dest_path % (language_code, base_name), 'w').write (match.group (0))
