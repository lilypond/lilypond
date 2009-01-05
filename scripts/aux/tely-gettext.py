#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Temporary script that helps translated docs sources conversion
# for texi2html processing

# USAGE:  tely-gettext.py PYTHON-DIR LOCALEDIR LANG FILES

print "tely-gettext.py"

import sys
import re
import os
import gettext

if len (sys.argv) > 3:
    buildscript_dir, localedir, lang = sys.argv[1:4]
else:
    print """USAGE:  tely-gettext.py PYTHON-DIR LOCALEDIR LANG FILES
  For example scripts/aux/tely-gettext.py python/out Documentation/po/out-www de Documentation/de/user/*.tely"""
    sys.exit (1)

sys.path.append (buildscript_dir)
import langdefs

double_punct_char_separator = langdefs.LANGDICT[lang].double_punct_char_sep
t = gettext.translation('lilypond-doc', localedir, [lang])
_doc = t.gettext

include_re = re.compile (r'@include (.*?)$', re.M)
whitespaces = re.compile (r'\s+')
ref_re = re.compile (r'(?ms)@(ruser|rprogram|ref|rlearning)\{(.*?)\}')
node_section_re = re.compile (r'@node (.*?)\n@((?:unnumbered|appendix)(?:(?:sub){0,2}sec)?|top|chapter|(?:sub){0,2}section|(?:major|chap|(?:sub){0,2})heading) (.*?)\n')
menu_entry_re = re.compile (r'\* (.*?)::')

def ref_gettext (m):
    r = whitespaces.sub (' ', m.group (2))
    return '@' + m.group (1) + '{' + _doc (r) + '}'

def node_gettext (m):
    return '@node ' + _doc (m.group (1)) + '\n@' + \
        m.group (2) + ' ' + _doc (m.group (3)) + \
	'\n@translationof ' + m.group (1) + '\n'

def menu_entry_gettext (m):
    return '* ' + _doc (m.group (1)) + '::'

def process_file (filename):
    print "Processing %s" % filename
    f = open (filename, 'r')
    page = f.read ()
    f.close()
    page = node_section_re.sub (node_gettext, page)
    page = ref_re.sub (ref_gettext, page)
    page = menu_entry_re.sub (menu_entry_gettext, page)
    page = page.replace ("""-- SKELETON FILE --
When you actually translate this file, please remove these lines as
well as all `UNTRANSLATED NODE: IGNORE ME' lines.""", """@c -- SKELETON FILE --""")
    page = page.replace ('UNTRANSLATED NODE: IGNORE ME', "@c UNTRANSLATED NODE: IGNORE ME")
    includes = [whitespaces.sub ('', f) for f in include_re.findall (page)]
    f = open (filename, 'w')
    f.write (page)
    f.close ()
    dir = os.path.dirname (filename)
    for file in includes:
        p = os.path.join (dir, file)
        if os.path.exists (p):
            process_file (p)

for filename in sys.argv[4:]:
    process_file (filename)
