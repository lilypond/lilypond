#!@PYTHON@
# texi-gettext.py

# USAGE:  texi-gettext.py [-o OUTDIR] BUILDSCRIPT-DIR LOCALEDIR LANG FILES
#
# -o OUTDIR specifies that output files should be written in OUTDIR
#    rather than be overwritten
#

import sys
import re
import os
import getopt
import gettext

optlist, args = getopt.getopt(sys.argv[1:],'o:')
buildscript_dir, localedir, lang = args[0:3]

outdir = '.'
for x in optlist:
	if x[0] == '-o':
		outdir = x[1]

sys.path.append (buildscript_dir)
import langdefs

double_punct_char_separator = langdefs.LANGDICT[lang].double_punct_char_sep
t = gettext.translation('lilypond-doc', localedir, [lang])
_doc = t.gettext

include_re = re.compile (r'@include (.*?)$', re.M)

def title_gettext (m):
	return '@' + m.group (1) + m.group (2) + _doc (m.group (3)) + m.group (4)

def process_file (filename):
	print "Processing %s" % filename
	f = open (filename, 'r')
	page = f.read ()
	f.close()
	page = re.sub (r'(?L)@(rglos|(?:unnumbered|appendix)(?:(?:sub){0,2}sec)?|top|chapter|(?:sub){0,2}section|(?:major|chap|(?:sub){0,2})heading)(\{| )(.*?)(\}|\n)', title_gettext, page)
	page = page.replace ("""-- SKELETON FILE --
When you actually translate this file, please remove these lines as
well as all `UNTRANSLATED NODE: IGNORE ME' lines.""", '')
	page = page.replace ('UNTRANSLATED NODE: IGNORE ME', _doc ("This section has not been translated yet; please refer to the manual in English."))
	f = open (os.path.join (outdir, filename), 'w')
	f.write (page)
	f.close ()
	dir = os.path.dirname (filename)
	for file in include_re.findall (page):
		p = os.path.join (dir, file)
		if os.path.exists (p):
			process_file (p)

for filename in args[3:]:
	process_file (filename)

toto = raw_input ("Press enter to continue")
