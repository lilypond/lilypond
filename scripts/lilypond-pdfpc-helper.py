#!@PYTHON@
import re
import getopt
import sys
import os

version = '@TOPLEVEL_VERSION@'

localedir = '@localedir@'
try:
	import gettext
	gettext.bindtextdomain ('lilypond', localedir)
	gettext.textdomain ('lilypond')
	_ = gettext.gettext
except:
	def _ (s):
		return s
	
program_name = os.path.basename (sys.argv[0])
print os.getcwd()
def usage():
	sys.stdout.write ('Usage: %s FILE:LINE:COLUMN' % program_name)
	sys.stdout.write ('\n\n')
	sys.stdout.write ('Call remote editor given Mozilla remote link command')
	sys.stdout.write ('\n\n')
	sys.stdout.write (_ ("Report bugs to %s.") % "bug-lilypond@gnu.org")
	sys.stdout.write ('\n')

def print_version ():
	sys.stdout.write (program_id ())
	sys.stdout.write ('\n')
	sys.stdout.write (_ ("""\
This program is free software.  It is covered by the GNU General Public
License and you are welcome to change it and/or distribute copies of it
under certain conditions.  Invoke as `%s --warranty' for more
information.
""") % "lilypond")

def program_id ():
	return '%s (GNU LilyPond) %s' % (program_name, version)

################################################################
(options, files) = getopt.getopt (sys.argv[1:], 'hv', ['help','version'])

for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--help' or o == '-h':
		usage ()
		sys.exit (0)
	if o == '--version' or o == '-v':
		print_version ()
		sys.exit (0)


if not files:
	usage()
	sys.exit (1)
	

################################################################

match = re.match ('([^:]+):([^:]+):(.*)', files[0])
if not match:
	sys.stderr.write (_("Not in FILE:LINE:COL format: ")
			  + files[0])
	sys.exit (1)

(file, line, column) = tuple (match.groups())

editor = os.environ['EDITOR']
ly_pc_editor = None
try:
	ly_pc_editor = os.environ['LYEDITOR']
except KeyError:
	pass


if ly_pc_editor == None:
	if  re.search ("emacs", editor):
		ly_pc_editor = 'emacsclient --no-wait +%(line)s:%(column)s %(file)s'
	elif re.search ('gvim', editor):
		ly_pc_editor = 'gvim --remote +:%(line)s:norm%(column)s %(file)s'
	elif re.search ('nedit', editor):
		ly_pc_editor = 'nc -noask +%(line)s %(file)s'

command = ly_pc_editor % vars()

status = os.system (command)
if status:
	sys.stderr.write (_("Command failed: `%s' (status %d)") % (command, status) + '\n')



