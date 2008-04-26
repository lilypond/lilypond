#!@PYTHON@
# -*- coding: utf-8 -*-
# texi-gettext.py

# USAGE:  texi-gettext.py [-o OUTDIR] LANG FILES
#
# -o OUTDIR specifies that output files should rather be written in OUTDIR
#

print "texi_gettext.py"

import sys
import re
import os
import getopt

import langdefs

optlist, args = getopt.getopt (sys.argv[1:],'o:')
lang = args[0]
files = args[1:]

outdir = '.'
for x in optlist:
    if x[0] == '-o':
        outdir = x[1]

double_punct_char_separator = langdefs.LANGDICT[lang].double_punct_char_sep
_doc = langdefs.translation[lang]

include_re = re.compile (r'@include ((?!../lily-).*?)\.texi$', re.M)
whitespaces = re.compile (r'\s+')
ref_re = re.compile (r'(?ms)@(rglos|ruser|rprogram|ref)(\{)(.*?)(\})')
node_section_re = re.compile (r'@(node|(?:unnumbered|appendix)(?:(?:sub){0,2}sec)?|top|chapter|(?:sub){0,2}section|(?:major|chap|(?:sub){0,2})heading)( )(.*?)(\n)')
menu_entry_re = re.compile (r'\* (.*?)::')

def title_gettext (m):
    if m.group (2) == '{':
        r = whitespaces.sub (' ', m.group (3))
    else:
        r = m.group (3)
    return '@' + m.group (1) + m.group (2) + _doc (r) + m.group (4)

def menu_entry_gettext (m):
    return '* ' + _doc (m.group (1)) + '::'

def include_replace (m, filename):
    if os.path.exists (os.path.join (os.path.dirname (filename), m.group(1)) + '.texi'):
        return '@include ' + m.group(1) + '.pdftexi'
    return m.group(0)

def process_file (filename):
    print "Processing %s" % filename
    f = open (filename, 'r')
    page = f.read ()
    f.close()
    page = node_section_re.sub (title_gettext, page)
    page = ref_re.sub (title_gettext, page)
    page = menu_entry_re.sub (menu_entry_gettext, page)
    page = page.replace ("""-- SKELETON FILE --
When you actually translate this file, please remove these lines as
well as all `UNTRANSLATED NODE: IGNORE ME' lines.""", '')
    page = page.replace ('UNTRANSLATED NODE: IGNORE ME', _doc ("This section has not been translated yet; please refer to the manual in English."))
    includes = include_re.findall (page)
    page = include_re.sub (lambda m: include_replace (m, filename), page)
    p = os.path.join (outdir, filename) [:-4] + 'pdftexi'
    f = open (p, 'w')
    f.write (page)
    f.close ()
    dir = os.path.dirname (filename)
    for file in includes:
        p = os.path.join (dir, file) + '.texi'
        if os.path.exists (p):
            process_file (p)

for filename in files:
    process_file (filename)
