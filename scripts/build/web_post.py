#!@PYTHON@

## This is web_post.py. This script deals with translations
## in the "make website" target.

import sys
import os
import glob

#indir, outdir = sys.argv[1:]

# FIXME: looks dangerous!
indir = sys.argv[1]
outdir=indir

html_files = glob.glob( os.path.join(indir, '*.html') )


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

	outfile = open( os.path.join(outdir, out_filename), 'w')
	for line in lines:
		# avoid external links
		if ((line.find("href") > 0) and (line.find("http")==-1)):
# eventually we want to do this, but I can't get it to work.
# waiting for help with apache (?)
#			line = line.replace(".html", "."+lang)
			line = line.replace(".html", "."+lang+".html")
		outfile.write(line)
	outfile.close()

