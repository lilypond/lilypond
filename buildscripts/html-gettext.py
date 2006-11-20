#!@PYTHON@
# html-gettext.py

# Usage:  html-gettext.py [-o OUTDIR] LOCALEDIR LANG FILES
#
# -o OUTDIR specifies that output files should be written in OUTDIR
#    rather than be overwritten
#
# LANG
# LOCALEDIR should contain 'lilypond-doc' message catalogs


### DATA
# Currently, typo_rules[LANG] only defines the HTML-coded space occuring
# before double punctuation characters (i.e. : ; ? ! ) for LANG

typo_rules = { 'fr':'&nbsp;', 'default':'' }


### PROGRAM

import sys
import re
import os
import string
import gettext
import getopt

optlist, args = getopt.getopt(sys.argv[1:],'o:')

outdir = '.'
for x in optlist:
	if x[0] == '-o':
		outdir = x[1]

if args[1] in typo_rules.keys():
	dbl_punct_char_separator = typo_rules[args[1]]
else:
	dbl_punct_char_separator = typo_rules['default']

t = gettext.translation('lilypond-doc', args[0], [args[1]])
_ = t.gettext

def link_gettext (m):
	return '<link rel="' + m.group(1) + '" ' + m.group(2) + ' title="' + _(m.group(3)) + '">'

def title_gettext (m):
	return '<title>' + _(m.group(1)) + ' - ' + m.group(2) + '</title>'

def a_href_gettext (m):
	if m.group(4) == ':':
		s = dbl_punct_char_separator + ':'
	elif m.group(4) == None:
		s = ''
	return '<a ' + (m.group(1) or '') + m.group(2) + _(m.group(3)) + '</a>' + s

def h_gettext (m):
	return '<h' + m.group(1) + ' class="' + m.group(2) + '">' + \
	       (m.group(3) or '') + _(m.group(4)) + '</h' + m.group(1) + '>'

for filename in args[2:]:
	f = open (filename, 'r')
	page = f.read ()
	f.close()
	page = re.sub (r'<link rel="(up|prev|next)" (.*?) title="([^"]*?)">', link_gettext, page)
	page = re.sub (r'<title>([^<]*?) - ([^<]*?)</title>', title_gettext, page)
	page = re.sub (r'<a ((?:rel="\w+")? ?(?:accesskey="[^"]+?" ?)?)(href="[^"]+?">)([^<]+)</a>(:)?', a_href_gettext, page)
	page = re.sub (r'<h(\d) class="(\w+)">([\d.]+ )?([^<]+)</h\1>', h_gettext, page)
	for w in ('Next:', 'Previous:', 'Up:'):
		page = re.sub (w, _(w), page)
	f = open (os.path.join (outdir, filename), 'w')
	f.write (page)
	f.close ()
