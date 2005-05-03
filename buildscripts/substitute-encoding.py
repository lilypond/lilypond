#!@PYTHON@
#
# mess up the coding table of PFAemmentaler to work around
# GS bug,
#
# http://bugs.ghostscript.com/show_bug.cgi?id=688017

import string
import sys
import re
import os

filename = sys.argv[1]
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
	glyph_names = (get_glyph_names ('out/feta20.enc') +
		       get_glyph_names ('out/feta-alphabet20.enc') +
		       get_glyph_names ('out/parmesan20.enc'))

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



