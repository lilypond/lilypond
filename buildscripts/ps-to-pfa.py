#!@PYTHON@

# ps-to-pfa.py -- make PostScript Type 3 font from separate ps char files
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>

name = 'ps-to-pfa'
version = '0.2'

outdir = 'out/'
datadir = ''

import os
import sys

import getopt
from string import *
import regex
import regsub
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
                      % (program_name)
		      )
    sys.exit (0)

identify ()
(options, files) = getopt.getopt (
    sys.argv[1:], 'd:', ['help', 'package'])
for opt in options:
    o = opt[0]
    a = opt[1]
    if o== '--help' or o == '-h':
	help ()
    elif o == '-d' or o == '--datadir':
	datadir = a
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
# urg ?
font = os.path.basename (os.path.splitext (mf)[0])
sys.stderr.write ('Font: %s\n'% font)

def header (f):
	f.write ('%!PS-AdobeFont-3.0: ' + font + '\n')
	f.write ('%%%%Creator: %s-%s\n' % (name, version))
	f.write ('\n')
	f.write ('/setgray { 1 add } bind def\n'
		'\n'
'8 dict begin\n'
'/FontType 3 def                             %% Required elements of font\n'
'/FontName /%s def\n'
'/FontMatrix [.001 0 0 .001 0 0] def\n'
'%%/FontMatrix [.01 0 0 .01 0 0] def\n'
'%%/FontMatrix [0.1 0 0 0.1 0 0] def\n'
'%% /FontBBox [-1000 -1000 1000 1000] def\n'
' /FontBBox [-3000 -3000 3000 3000] def\n'
'%% /FontBBox [-300 -300 300 300] def\n'
'%%/FontBBox [-30 -30 30 30] def\n'
'\n'
'/Encoding 256 array def                     %% Trivial encoding vector\n'
'0 1 255 {Encoding exch /.notdef put} for\n' % (font))

def footer (f):
	f.write ('\n'
'/BuildGlyph {                               % Stack contains: font charname\n'
'%  1000 0                                   % Width\n'
'%  -750 -750 750 750                        % Bounding Box\n'
'  3000 0                                    % Width\n'
'  -3000 -3000 3000 3000                     % Bounding Box\n'
'%  300 0                                    % Width\n'
'%  -300 -300 300 300                        % Bounding Box\n'
'%  30 0                                     % Width\n'
'%  -30 -30 30 30                            % Bounding Box\n'
'  setcachedevice\n'
'  exch /CharProcs get exch                  % Get CharProcs dictionary\n'
'  2 copy known not {pop /.notdef} if        % See if charname is known\n'
'  get exec                                  % Execute character procedure\n'
'} bind def\n'
'\n'
'/BuildChar {                                % Level 1 compatibility\n'
'  1 index /Encoding get exch get\n'
'  1 index /BuildGlyph get exec\n'
'} bind def\n'
'\n'
'currentdict\n'
'end                                         % of font dictionary\n')
	f.write ('\n')
	f.write ('/%s\n' % font)
	f.write (''
'exch definefont pop                         % Define the font\n')

def characters (f):
	#urg
	# chars = os.listdir ()
	# chars.sort ()
	sys.stderr.write ('[')
	pipe = os.popen ('/bin/ls -1 ' + font + '.[0-9] ' + font + '.[0-9][0-9] ' + font + '.[0-9][0-9][0-9] 2> /dev/null')
	chars = []
	i = pipe.readline ()
	while i:
		chars.append (i[0:len (i)-1])
		i = pipe.readline ()
	f.write ('\n'
'/CharProcs 3 dict def                       % Subsidiary dictiorary for\n'
'CharProcs begin                             % individual character definitions\n')
	charprocs = '  /.notdef {} def\n'
	encoding = ''
	for i in chars: 
		s = gulp_file (i)
		s = regsub.gsub ('^%.*\n', '', s)
		s = regsub.gsub ('^showpage\n', '', s)
		s = regsub.gsub ('^', '    ', s)
		n = atoi (regsub.gsub ('.*\.', '', i))
		s = '\n  /%s-%d{\n%s} bind def\n' % (font, n, s)
		encoding = encoding + 'Encoding %d /%s-%d put\n' % (n, font, n)
		charprocs = charprocs + s
	f.write (charprocs)
	f.write ('\n')
	f.write ('end                                         % of CharProcs\n')
	f.write (encoding)
	f.write ('\n')
	sys.stderr.write (']')

ps = outdir + font + '.pfa'
ps_file = open (ps, 'w')
header (ps_file)
characters (ps_file)
footer (ps_file)
sys.stderr.write ('\n')
ps_file.close ()
sys.stderr.write ('Wrote PostScript font: %s\n'% ps)

