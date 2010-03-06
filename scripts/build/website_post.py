#!@PYTHON@
#-*- coding: utf-8 -*-

## This is web_post.py. This script deals with translations
## in the "make website" target.

import sys
import os
import glob

#### Translation data
lang_lookup = {
  'fr': 'français',
  'es': 'español',
  '': 'english'
}

lang_other_langs = {
  'es': 'Otros idiomas: ',
  'fr': 'lirer des autres langs: ',
  '': 'Other languages: '
}


#### Actual program

#indir, outdir = sys.argv[1:]

# FIXME: looks dangerous!
indir = sys.argv[1]
outdir=indir

html_files = glob.glob( os.path.join(indir, '*.html') )

# messy way to get all languages
langs_set = set()
for file in html_files:
	file_split = file.split('.')
	if (len(file_split) == 2):
		# it's English
		lang = ''
	else:
		# it's a translation
		lang = file_split[1]
	langs_set.add(lang)
langs = list(langs_set)
langs.sort()

def makeFooter(currentLang, currentPage):
	text = "<p id=\"languages\">\n"
	text += lang_other_langs[currentLang]
	for i in range(len(langs)):
		l = langs[i]
		if (l == currentLang):
			continue
		text += "<a href=\""
		text += currentPage
		if (not (l=="")):
			text += "." + l
		text += ".html\">"
		text += lang_lookup[l]
		text += "</a>"
		if (i < len(langs)-2):
			text += ", "
		else:
			text += ".\n"
	# TODO: add link to automatic language selection?
	# still need to include this page in the new webpages somewhere
	text += "</p>\n"
	return text


for file in html_files:
	file_split = file.split('.')
	# we want to strip the .html
	out_filename = os.path.basename(file_split[0])
	if (len(file_split) == 2):
		# it's English
		lang = ''
	else:
		# it's a translation
		lang = file_split[1]
	out_filename += '.'+lang

# I can't get the previous name to work
	out_filename = os.path.basename(file)

	# translation links should point to translations
	lines = open(file).readlines()
	# ick
	os.remove(file)

	# ick
	lang_footer = makeFooter(lang, out_filename.split('.')[0])
	
	outfile = open( os.path.join(outdir, out_filename), 'w')
	for line in lines:
		# avoid external links
		if ((line.find("href") >= 0) and (line.find("http")==-1)):
# eventually we want to do this, but I can't get it to work.
# waiting for help with apache (?)
#			line = line.replace(".html", "."+lang)
			text = ""
			if (not (lang=="")):
				text += "." + lang
			text += ".html"
			line = line.replace(".html", text)
		if ((line.find("href") >= 0) and
		    (line.find("http")==-1) and
		    (line.find("pdf") >= 0)):
			text = ""
			if (not (lang=="")):
				text += "." + lang
			text += ".pdf"
			line = line.replace(".pdf", text)


		if (line.find("<!-- FOOTER -->") >= 0):
			outfile.write( lang_footer )
		outfile.write(line)
	outfile.close()

