#!@PYTHON@
# html-gettext.py

# USAGE:  html-gettext.py [-o OUTDIR] BUILDSCRIPT-DIR LOCALEDIR LANG FILES
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
print localedir
print lang
t = gettext.translation('lilypond-doc', localedir, [lang])
my_gettext = t.gettext

html_codes = ((' -- ', ' &ndash; '),
	      (' --- ', ' &mdash; '))

def _ (s):
	for c in html_codes:
		s = s.replace (c[1], c[0])
	s = my_gettext (s)
	for c in html_codes:
		s = s.replace (c[0], c[1])
	return s

def link_gettext (m):
	return '<link rel="' + m.group(1) + '" ' + m.group(2) + ' title="' + _(m.group(3)) + '">'

def title_gettext (m):
	return '<title>' + _(m.group(1)) + ' - ' + m.group(2) + '</title>'

def a_href_gettext (m):
	if m.group(6) == ':':
		s = double_punct_char_separator + ':'
	elif m.group(6) == None:
		s = ''
	return '<a ' + (m.group(1) or '') + m.group(2) + m.group(3) + _(m.group(4)) + m.group(5) + '</a>' + s

def h_gettext (m):
	return '<h' + m.group(1) + m.group(2) + '>' + \
	       m.group(3) + _(m.group(4)) + '</h' + m.group(1) + '>'

for filename in args[3:]:
	f = open (filename, 'r')
	page = f.read ()
	f.close()
	page = re.sub (r'<link rel="(up|prev|next)" (.*?) title="([^"]*?)">', link_gettext, page)
	page = re.sub (r'<title>([^<]*?) - ([^<]*?)</title>', title_gettext, page)
	page = re.sub (r'<a ((?:rel="\w+")? ?(?:accesskey="[^"]+?" ?)?)(href="[^"]+?">)((?:<code>|)(?:[\d.]+ |))([^<]+)(</code>|)</a>(:)?', a_href_gettext, page)
	page = re.sub (r'<h(\d)( class="\w+"|)>([\d.]+ |)?([^<]+)</h\1>', h_gettext, page)
	for w in ('Next:', 'Previous:', 'Up:'):
		page = re.sub (w, _(w), page)
	f = open (os.path.join (outdir, filename), 'w')
	f.write (page)
	f.close ()
