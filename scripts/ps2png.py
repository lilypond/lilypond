#!@PYTHON@

import getopt
import glob
import os
import string
import sys

datadir = '@local_lilypond_datadir@'
if not os.path.isdir (datadir):
	datadir = '@lilypond_datadir@'
if os.environ.has_key ('LILYPONDPREFIX') :
	datadir = os.environ['LILYPONDPREFIX']
	while datadir[-1] == os.sep:
		datadir= datadir[:-1]

sys.path.insert (0, os.path.join (datadir, 'python'))

# Customize these
#if __name__ == '__main__':

import lilylib as ly
global _;_=ly._
global re;re = ly.re

preview_resolution = 90
# Use /etc/papersize?
preview_papersize = "a4"

# lilylib globals
program_name = os.path.basename (sys.argv[0])
verbose_p = 0
program_version = '@TOPLEVEL_VERSION@'
pseudo_filter_p = 0 # ugr.

help_summary = _ ("Convert PostScript to PNG image.")
copyright = ('Han-Wen Nienhuys <hanwen@cs.uu.nl',
	     'Jan Nieuwenhuizen <janneke@gnu.org')

option_definitions = [
	('', 'h', 'help', _ ("print this help")),
	('', 'V', 'verbose', _ ("be verbose")),
	(_ ('PAPER'), 'P', 'papersize', _ ("use papersize PAPER")),
	(_ ('RES'), 'R', 'resolution', _ ("set the resolution of the preview to RES")),
	]

(sh, long) = ly.getopt_args (option_definitions)
try:
	(options, files) = getopt.getopt (sys.argv[1:], sh, long)
except getopt.error, s:
	sys.stderr.write ('\n')
	ly.error (_ ("getopt says: `%s\'" % s))
	sys.stderr.write ('\n')
	ly.help ()
	ly.exit (2)
	
for opt in options:
	o = opt[0]
	a = opt[1]

	if o == '--help' or o == '-h':
		ly.identify (sys.stdout)
		ly.help ()
		sys.exit (0)
	elif o == '--papersize' or o == '-P':
		preview_papersize = a
	elif o == '--resolution' or o == '-R':
		preview_resolution = string.atoi (a)
	elif o == '--verbose' or o == '-V':
		verbose_p = 1

ly.identify (sys.stderr)
for f in files:
	outfiles = ly.make_ps_images (f, resolution = preview_resolution,
				      papersize = preview_papersize)
	sys.stderr.write (_ ("Wrote `%s'" % string.join (outfiles, ', ')))
	sys.stderr.write ('\n')
