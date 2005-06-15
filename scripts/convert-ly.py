#!@PYTHON@
#
# convert-ly.py -- Update old LilyPond input files (fix name?)
#
# source file of the GNU LilyPond music typesetter
#
# (c) 1998--2005  Han-Wen Nienhuys <hanwen@cs.uu.nl>
#                 Jan Nieuwenhuizen <janneke@gnu.org>

import os
import sys
import __main__
import getopt
import string

datadir = '@local_lilypond_datadir@'
if not os.path.isdir (datadir):
	datadir = '@lilypond_datadir@'
if os.environ.has_key ('LILYPONDPREFIX'):
	datadir = os.environ['LILYPONDPREFIX']
	while datadir[-1] == os.sep:
		datadir= datadir[:-1]

sys.path.insert (0, os.path.join (datadir, 'python'))

import lilylib as ly
import fontextract
global _;_=ly._
global re;re = ly.re

from convertrules import *


help_summary = _ (
'''Update LilyPond input to newer version.  By default, update from the
version taken from the \\version command, to the current LilyPond version.

Examples:

  convert-ly -e old.ly
  convert-ly --from=2.3.28 --to 2.5.21 foobar.ly
''')

copyright = ('Jan Nieuwenhuizen <janneke@gnu.org>',
	     'Han-Wen Nienhuys <hanwen@cs.uu.nl>')

option_definitions = [
	('', 'e', 'edit',_('edit in place')),
	(_('VERSION'), 'f', 'from',
	 _('start from VERSION [default: \\version found in file]')),
	('', 'h', 'help',_('print this help')),
	('',  'n', 'no-version',_ ('do not add \\version command if missing')),
	('','s', 'show-rules', _('print rules [default: --from=0, --to=@TOPLEVEL_VERSION@]')),
	(_('VERSION'), 't', 'to',_('convert to VERSION [default: @TOPLEVEL_VERSION@]')),
	('','v','version',_('print program version"'))
]	

program_name = os.path.basename (sys.argv[0])
program_version = '@TOPLEVEL_VERSION@'


add_version = 1

def guess_lilypond_version (filename):
	s = open (filename).read ()
	m = lilypond_version_re.search (s)
	if m:
		return m.group (1)
	else:
		return ''

class FatalConversionError:
	pass

conversions = []

class UnknownVersion:
	pass

def do_one_file (infile_name):
	sys.stderr.write (_ ("Processing `%s\'... ") % infile_name)
	sys.stderr.write ('\n')
	outfile_name = ''
	if __main__.edit:
		outfile_name = infile_name + '.NEW'
	elif __main__.outfile_name:
		outfile_name = __main__.outfile_name

	if __main__.from_version:
		from_version = __main__.from_version
	else:
		guess = guess_lilypond_version (infile_name)
		if not guess:
			raise UnknownVersion ()
		from_version = str_to_tuple (guess)

	if __main__.to_version:
		to_version = __main__.to_version
	else:
		to_version = latest_version ()


	if infile_name:
		infile = open (infile_name, 'r')
	else:
		infile = sys.stdin


	(last, result) = do_conversion (infile.read (), from_version, to_version)
	infile.close ()


	if last:
		newversion = r'\version "%s"' % tup_to_str (last)
		if lilypond_version_re.search (result):
			result = re.sub (lilypond_version_re_str,
					 '\\' + newversion, result)
		elif add_version:
			result = newversion + '\n' + result
			
		error_file.write ('\n')			
	
		if __main__.edit:
			try:
				os.remove(infile_name + '~')
			except:
				pass
			os.rename (infile_name, infile_name + '~')
			outfile = open (infile_name, 'w')
		else:
			outfile = sys.stdout


		outfile.write (result)

	sys.stderr.flush ()

edit = 0
to_version = ()
from_version = ()
outfile_name = ''
show_rules_p = 0

def do_options ():
	global from_version, to_version, edit, show_rules_p, add_version

	(sh, long) = ly.getopt_args (option_definitions)
	try:
		(options, files) = getopt.getopt (sys.argv[1:], sh, long)
	except getopt.error, s:
		sys.stderr.write ('\n')
		ly.error (_ ("getopt says: `%s'" % s))
		sys.stderr.write ('\n')
		ly.help ()
		ly.exit (2)

	for opt in options:
		o = opt[0]
		a = opt[1]

		if o == '--help' or o == '-h':
			ly.help ()
			sys.exit (0)
		elif o == '--version' or o == '-v':
			ly.identify (sys.stdout)
			sys.exit (0)
		elif o== '--from' or o=='-f':
			from_version = str_to_tuple (a)
		elif o== '--to' or o=='-t':
			to_version = str_to_tuple (a)
		elif o== '--edit' or o == '-e':
			edit = 1
		elif o== '--show-rules' or o == '-s':
			show_rules_p = 1
		elif o == '--no-version' or o == '-n':
			add_version = 0
		else:
			print o
			raise getopt.error

	return files


def main ():
	files = do_options ()

	# should parse files[] to read \version?
	if show_rules_p:
		show_rules (sys.stdout, from_version, to_version)
		sys.exit (0)

	ly.identify (sys.stderr)

	if not files:
		ly.help ()
		sys.exit (2)

	for f in files:
		if f == '-':
			f = ''
		elif not os.path.isfile (f):
			ly.error (_ ("can't open file: `%s'") % f)
			if len (files) == 1:
				sys.exit (1)
			continue
		try:
			do_one_file (f)
		except UnknownVersion:
			ly.error (_ ("can't determine version for `%s'. Skipping") % f)

	sys.stderr.write ('\n')

main ()
