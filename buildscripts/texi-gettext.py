#!@PYTHON@
# -*- coding: utf-8 -*-
# texi-gettext.py

# USAGE:  texi-gettext.py [-o OUTDIR] BUILDSCRIPT-DIR LOCALEDIR LANG FILES
#
# -o OUTDIR specifies that output files should rather be written in OUTDIR
#

print "texi_gettext.py"

import sys
import re
import os
import getopt
import gettext

optlist, args = getopt.getopt (sys.argv[1:],'o:')
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

include_re = re.compile (r'@include ((?!lily-).*?)\.texi$', re.M)
whitespaces = re.compile (r'\s+')
ref_re = re.compile (r'(?ms)@(rglos|ref)(\{)(.*?)(\})')
node_section_re = re.compile (r'@(node|(?:unnumbered|appendix)(?:(?:sub){0,2}sec)?|top|chapter|(?:sub){0,2}section|(?:major|chap|(?:sub){0,2})heading)( )(.*?)(\n)')
menu_entry_re = re.compile (r'\* (.*?)::')

# Why not use recode?
# - well, it would add one more dependency...
accents2texi = (
	("á", "@'a"),
	("à", "@`a"),
	("â", "@^a"),
	("ä", "@\"a"),
	("é", "@'e"),
	("è", "@`e"),
	("ê", "@^e"),
	("ë", "@\"e"),
	("ó", "@'o"),
	("ò", "@`o"),
	("ô", "@^o"),
	("ö", "@\"o"),
	("ú", "@'u"),
	("ù", "@`u"),
	("û", "@^u"),
	("ü", "@\"u"),
	("ç", "@,{c}"),
	("À", "@`A"),
	("Á", "@'A"),
	("Â", "@^A"),
	("Ä", "@\"A"),
	("É", "@'E"),
	("È", "@`E"),
	("Ê", "@^E"),
	("Ë", "@\"E"),
	("Ó", "@'O"),
	("Ò", "@`O"),
	("Ô", "@^O"),
	("Ö", "@\"O"),
	("Ú ", "@'U"),
	("Ù", "@`U"),
	("Û", "@^U"),
	("Ü", "@\"U"),
	("Ç", "@,{C}"),
	("Í", "@'{@dotless{i}}"),
	("ì", "@`{@dotless{i}}"),
	("î", "@^{@dotless{i}}"),
	("ï", "@\"{@dotless{i}}"),
	("Í", "@'I"),
	("Ì", "@`I"),
	("Î", "@^I"),
	("Ï", "@\"I"),
	("œ", "@oe{}"),
	("Œ", "@OE{}"),
	("æ", "@ae{}"),
	("Æ", "@AE{}"),
	("¡", "@exclamdown{}"),
	("¿", "@questiondown{}"),
	("ø", "@o{}"),
	("Ø", "@O{}"),
	("ß", "@ss{}"),
	("ł", "@l{}"),
	("Ł", "@L{}"),
	("å", "@aa{}"),
	("Å", "@AA{}"))


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
	for (u_char, texiaccent_char) in accents2texi:
		page = page.replace (u_char, texiaccent_char)
	p = os.path.join (outdir, filename) [:-4] + 'pdftexi'
	f = open (p, 'w')
	f.write (page)
	f.close ()
	dir = os.path.dirname (filename)
	for file in includes:
		p = os.path.join (dir, file) + '.texi'
		if os.path.exists (p):
			process_file (p)

for filename in args[3:]:
	process_file (filename)
