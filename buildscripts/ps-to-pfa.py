#!@PYTHON@

# ps-to-pfa.py -- make PostScript Type 3 font from separate ps char files
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>

#TODO.  This could be more efficient.

name = 'ps-to-pfa'
version = '0.4'

datadir = ''

import find
import os
import sys
import getopt
from string import *
import re
import time

def program_id ():
    return name + ' version ' + version;

def identify ():
    sys.stdout.write (program_id () + '\n')

def help ():
    sys.stdout.write ("Usage: %s [options] [files]\n"
		      "ps-to-pfa.py -- make PostScript Type 3 font from separate ps char files\n\n"
                      + "Options:\n"
                      + "  -d, --datadir=DIR      use DIR for ps header/footer\n"
                      + "  -h, --help             print this help\n"
		      + "  -o, --output=FILE      set output file to FILE.\n"
                      % (program_name)
		      )
    sys.exit (0)

output_name = ''

identify ()
(options, files) = getopt.getopt (
    sys.argv[1:], 'o:d:', ['help', 'package', 'output='])
for opt in options:
    o = opt[0]
    a = opt[1]
    if o== '--help' or o == '-h':
	help ()
    elif o == '-d' or o == '--datadir':
	datadir = a
    elif o == '-o' or o =='--output':
	output_name = a
    else:
	print o
	raise getopt.error


def gulp_file (f):
	sys.stderr.write ('[%s' % f)
	try:
		i = open (f)
		i.seek (0, 2)
		n = i.tell ()
		i.seek (0,0)
	except:
		sys.stderr.write ('can\'t open file %s\n ' % f)
		return ''
	s = i.read (n)
	sys.stderr.write (']')
	if len (s) <= 0:
		sys.stderr.write ('gulped empty file: %s\n'% f)
	return s

mf = files[0]

input_name = mf
font_name = os.path.basename (os.path.splitext (mf)[0])
if not output_name:
    output_name  = font_name + '.pfa'


sys.stderr.write ('Font: %s\n'% font_name)

def header (f):
	f.write ('%!PS-AdobeFont-3.0: ' + font_name + '\n')
	f.write ('%%%%Creator: %s-%s\n' % (name, version))
	f.write ('\n')
	f.write (r"""
8 dict begin
/FontType 3 def                             %% Required elements of font
/FontName /%s def""" % font_name)
	f.write (r"""
/FontMatrix [.083 0 0 .083 0 0] def       %% why .83?
/FontBBox [-1000 -1000 1000 1000] def	  %% does not seem to matter.
/Encoding 256 array def                     %% Trivial encoding vector
0 1 255 {Encoding exch /.notdef put} for
""")
def footer (f):
	f.write (r"""
/BuildGlyph {                               % Stack contains: font charname
  1000 0                                   % Width
  -1000 -1000 1000 1000                    % Bounding Box
  setcachedevice
  exch /CharProcs get exch                  % Get CharProcs dictionary
  2 copy known not {pop /.notdef} if        % See if charname is known
  get exec                                  % Execute character procedure
} bind def

/BuildChar {                                % Level 1 compatibility
  1 index /Encoding get exch get
  1 index /BuildGlyph get exec
} bind def

currentdict
end                                         % of font dictionary
"""
)

	f.write ('/%s\n' % font_name)
	f.write (''
'exch definefont pop                         % Define the font\n')

suspect_re = re.compile ('closepath ((gsave )*fill( grestore stroke)*) 1 setgray newpath (.*?) closepath fill')

def characters (f):
	sys.stderr.write ('[')
	
	files = []
	import find			# q
	suffixes = [".[0-9]", ".[0-9][0-9]",  ".[0-9][0-9][0-9]"]
	for suf in suffixes:
		files = files + find.find (font_name + suf)


	# concat all files into charprocs.
	charprocs = '  /.notdef {} def\n'
	encoding = ''
	for i in files: 
		s = gulp_file (i)
		s = re.sub ('%[^\n]*\n', '', s)
		
		# if you want readable stuff, look at MP output.
		s = re.sub ('[\n\t ]+', ' ', s)
		s = re.sub ('showpage', '', s)

		# MP's implementation of unfill confuses GS.
		# Look for "Metapost & setgray" on deja.com
		# we do some twiddling to use eofill i.s.o. fill
		if re.search ('setgray',s ):
			m = suspect_re.search (s)
			while m:
				fill = m.group (1)
				path = m.group (4)

				# be complicated, in case of gsave/grestore.
				# vill as quick hack to avoid duple substitutions.
				fill = re.sub ('fill', 'eovill', fill, count = 1)
				s = re.sub (m.group (0), ' %s %s ' % (path, fill), s)
				m = suspect_re.search (s)

			s = re.sub ('eovill' , 'eofill', s)
			s = re.sub ('0 setgray' ,'', s)

			
		m = re.match ('.*\.([0-9]+)',i)
		n = atoi (m.group (1))

		s = '\n  /%s-%d{\n%s} bind def\n' % (font_name, n, s)
		encoding = encoding + 'Encoding %d /%s-%d put\n' % (n, font_name, n)
		charprocs = charprocs + s

	f.write ('\n'
'/CharProcs 3 dict def                       % Subsidiary dictiorary for\n'
'CharProcs begin                             % individual character definitions\n')
	f.write (charprocs)
	f.write ('\n')
	f.write ('end                                         % of CharProcs\n')
	f.write (encoding)
	f.write ('\n')
	sys.stderr.write (']')


ps_file = open (output_name, 'w')
header (ps_file)
characters (ps_file)
footer (ps_file)
sys.stderr.write ('\n')
ps_file.close ()
sys.stderr.write ('Wrote PostScript font: %s\n' % output_name)

