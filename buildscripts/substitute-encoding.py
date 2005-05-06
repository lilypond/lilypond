#!@PYTHON@
#
# mess up the coding table of PFAemmentaler to work around
# GS bug,
#
# http://bugs.ghostscript.com/show_bug.cgi?id=688017

import getopt
import os
import re
import string
import sys

outdir = 'out'
(options, files) = getopt.getopt (sys.argv[1:], 'o', ['outdir='])
for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--outdir' or o == '-o':
		outdir = a

filename = files[0]
def get_glyph_names (enc_name):
	enc_str = open (enc_name).read ()
	glyphs = []
	def note_glyph (match):
		nm = match.group (1)
		glyphs.append (nm)


	m = re.sub ("(/[a-z-.A-Z0-9]+) %", note_glyph, enc_str )

	glyphs = filter (lambda x: None == re.match (r'/\.notdef', x),
			 glyphs)
	return glyphs

if re.search ('mmental', filename):
	glyph_names = (get_glyph_names ('%(outdir)s/feta20.enc' % vars ())
		       + get_glyph_names ('%(outdir)s/feta-alphabet20.enc' % vars ())
		       + get_glyph_names ('%(outdir)s/parmesan20.enc' % vars ()))

	str = open (filename).read ()
	os.rename (filename, filename + "~")

	glyph_names = glyph_names[:256]
	
	put_glyphs = ''
	i = 0
	for g in glyph_names:
		put_glyphs += 'dup %d %s put \n' % (i,g)
		i += 1
		
	str = re.sub ('( 0 1 255 { 1 index exch /.notdef put} for)',
		      '\\1 \n %s'  % put_glyphs,
		      str)

	open (filename, 'w').write (str)
