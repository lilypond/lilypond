#!@PYTHON@
#
# update-lily.py -- lilypond autobuilder
# 
# source file of the GNU LilyPond music typesetter
#
# download and rebuild latest lilypond or from specified url
# 

'''
TODO:
    * more flexible build paths
    * cleanup previous tree
    * flexible build command
    * show only?
'''

import os
import fnmatch
import stat
import string
import re
import getopt
import sys
import __main__
import operator
import tempfile


sys.path.append ('@datadir@/python')
import gettext
gettext.bindtextdomain ('lilypond', '@localedir@')
gettext.textdomain('lilypond')
_ = gettext.gettext


program_name = 'build-lily'
package_name = 'lilypond'
help_summary = _("Fetch and rebuild from latest source package")
build_root = os.environ ['HOME'] + '/usr/src'
build_command = './configure; make web'
release_dir = build_root + '/releases'
patch_dir = build_root + '/patches'

url = 'file:/home/ftp/pub/gnu/LilyPond/development/lilypond-*.tar.gz'
url = 'ftp://appel.lilypond.org/pub/gnu/LilyPond/development/lilypond-*.tar.gz'
url = 'ftp://ftp.cs.uu.nl/pub/GNU/LilyPond/development/lilypond-*.tar.gz'


# lily_py.py -- options and stuff
# 
# source file of the GNU LilyPond music typesetter

# BEGIN Library for these?
# cut-n-paste from ly2dvi

program_version = '@TOPLEVEL_VERSION@'
if program_version == '@' + 'TOPLEVEL_VERSION' + '@':
	program_version = '1.3.142'


original_dir = os.getcwd ()
temp_dir = '%s.dir' % program_name
keep_temp_dir_p = 0
verbose_p = 0

def identify ():
	sys.stdout.write ('%s (GNU LilyPond) %s\n' % (program_name, program_version))

def warranty ():
	identify ()
	sys.stdout.write ('\n')
	sys.stdout.write (_ ('Copyright (c) %s by' % ' 2001'))
	sys.stdout.write ('\n')
	sys.stdout.write ('  Han-Wen Nienhuys')
	sys.stdout.write ('  Jan Nieuwenhuizen')
	sys.stdout.write ('\n')
	sys.stdout.write (_ (r'''
Distributed under terms of the GNU General Public License. It comes with
NO WARRANTY.'''))
	sys.stdout.write ('\n')

def progress (s):
	sys.stderr.write (s + '\n')

def warning (s):
	sys.stderr.write (_ ("warning: ") + s)
	sys.stderr.write ('\n')
	
		
def error (s):
	sys.stderr.write (_ ("error: ") + s)
	sys.stderr.write ('\n')
	raise _ ("Exiting ... ")

def getopt_args (opts):
	'''Construct arguments (LONG, SHORT) for getopt from  list of options.'''
	short = ''
	long = []
	for o in opts:
		if o[1]:
			short = short + o[1]
			if o[0]:
				short = short + ':'
		if o[2]:
			l = o[2]
			if o[0]:
				l = l + '='
			long.append (l)
	return (short, long)

def option_help_str (o):
	'''Transform one option description (4-tuple ) into neatly formatted string'''
	sh = '  '	
	if o[1]:
		sh = '-%s' % o[1]

	sep = ' '
	if o[1] and o[2]:
		sep = ','
		
	long = ''
	if o[2]:
		long= '--%s' % o[2]

	arg = ''
	if o[0]:
		if o[2]:
			arg = '='
		arg = arg + o[0]
	return '  ' + sh + sep + long + arg


def options_help_str (opts):
	'''Convert a list of options into a neatly formatted string'''
	w = 0
	strs =[]
	helps = []

	for o in opts:
		s = option_help_str (o)
		strs.append ((s, o[3]))
		if len (s) > w:
			w = len (s)

	str = ''
	for s in strs:
		str = str + '%s%s%s\n' % (s[0], ' ' * (w - len(s[0])  + 3), s[1])
	return str

def help ():
	sys.stdout.write (_ ("Usage: %s [OPTION]... FILE") % program_name)
	sys.stdout.write ('\n\n')
	sys.stdout.write (help_summary)
	sys.stdout.write ('\n\n')
	sys.stdout.write (_ ("Options:"))
	sys.stdout.write ('\n')
	sys.stdout.write (options_help_str (option_definitions))
	sys.stdout.write ('\n\n')
	sys.stdout.write (_ ("Report bugs to %s") % 'bug-gnu-music@gnu.org')
	sys.stdout.write ('\n')
	sys.exit (0)


def setup_temp ():
	global temp_dir
	if not keep_temp_dir_p:
		temp_dir = tempfile.mktemp (program_name)
	try:
		os.mkdir (temp_dir, 0777)
	except OSError:
		pass
		
	
def system (cmd, ignore_error = 0):
	if verbose_p:
		progress (_ ("Invoking `%s\'") % cmd)
	st = os.system (cmd)
	if st:
		msg =  ( _ ("error: ") + _ ("command exited with value %d") % st)
		if ignore_error:
			sys.stderr.write (msg + ' ' + _ ("(ignored)") + ' ')
		else:
			error (msg)

	return st


def cleanup_temp ():
	if not keep_temp_dir_p:
		if verbose_p:
			progress (_ ('Cleaning up `%s\'') % temp_dir)
		system ('rm -rf %s' % temp_dir)


def set_setting (dict, key, val):
	try:
		val = string.atof (val)
	except ValueError:
		#warning (_ ("invalid value: %s") % `val`)
		pass

	try:
		dict[key].append (val)
	except KeyError:
		warning (_ ("no such setting: %s") % `key`)
		dict[key] = [val]

# END Library

option_definitions = [
	('DIR', 'b', 'build-root', _ ("unpack and build in DIR [%s]") % build_root),
	('', 'h', 'help', _ ("this help")),
        ('', 'k', 'keep', _ ("keep all output, and name the directory %s") % temp_dir),
	('', 'V', 'verbose', _ ("verbose")),
	('', 'v', 'version', _ ("print version number")),
	('URL', 'u', 'url', _ ("fetch and build URL [%s]") % url),
	('', 'w', 'warranty', _ ("show warranty and copyright")),
	]

def list_file (user, passwd, host, dir, file):
	match = []
	for i in os.listdir (dir):
		if fnmatch.fnmatch (i, file):
			match.append (i)
	return match

list_ = list_file

#
# ugh: use ftp module.
#
def list_ftp (user, passwd, host, dir, file):
	if user == 'None':
		user = 'anonymous'
	if passwd == 'None':
		passwd = program_name

	command = '''
open -u%s,%s -p21 %s
set passive-mode off
cd "%s"
ls -1 "%s"
''' % (user, passwd, host, dir, file)
	temp = tempfile.mktemp (program_name)
	f = open (temp, 'w')
	f.write (command)
	f.close ()
	p = os.popen ('lftp -f %s' % temp, 'r')
	s = p.read ()
	status = p.close ()
	return string.split (s[:-1], '\n')
	
def split_url (url):
	m = re.match ('([^:/]*)(:)?(/*([^:]*):)?(/*([^@]*)@)?(//([^/]*))?(.*)/(.*)',
		      url)
	if not m:
		error ("can't parse url: %s " % url)
	return (m.group (1), m.group (4), m.group (6), m.group (8),
		m.group (9), m.group (10))
	
def list_url (url):
	s = "list_%s ('%s', '%s', '%s', '%s', '%s')" % split_url (url)
	return eval (s)

def version_tuple_to_str (t):
	if t[3]:
		my = '.%s%d' % (t[3], t[4])
	else:
		my = ''
	return ('%d.%d.%d' % t[0:3]) + my

def version_str_to_tuple (s):
	t = string.split (s, '.')
	if len (t) >= 4:
		my_name = t[3][:-1]
		my_number = string.atoi (t[3][-1])
	else:
		my_name = None
		my_number = None
	return (string.atoi (t[0]), string.atoi (t[1]), string.atoi (t[2]),
		my_name, my_number)

def split_package (p):
	m = re.match ('(.*)-([0-9]*.*).tar.gz', p)
	return (m.group (1), version_str_to_tuple (m.group (2)))

def join_package (t):
	return t[0] + '-' + version_tuple_to_str (t[1])

def copy_file (user, passwd, host, dir, file):
	os.system ('cp %s/%s .' % (dir, file))

copy_ = copy_file

def copy_ftp (user, passwd, host, dir, file):
	if user == 'None':
		user = 'anonymous'
	if passwd == 'None':
		passwd = program_name

	command = '''
open -u%s,%s -p21 %s
set passive-mode off
cd "%s"
get "%s"
''' % (user, passwd, host, dir, file)
	temp = tempfile.mktemp (program_name)
	f = open (temp, 'w')
	f.write (command)
	f.close ()
	p = os.popen ('lftp -f %s' % temp, 'r')
	s = p.read ()
	status = p.close ()
	
def copy_url (url, dir):
	os.chdir (dir)
	s = "copy_%s ('%s', '%s', '%s', '%s', '%s')" % split_url (url)
	eval (s)


def find_latest (url):
	progress (_ ("listing %s...") % url)
	list = map (split_package, list_url (url))
	list.sort ()
	return join_package (list[-1])

def build (p):
	os.chdir (build_root)
	system ('tar xzf %s/%s.tar.gz' % (release_dir, p))
	os.chdir (p)
	return system (build_command)

(sh, long) = getopt_args (__main__.option_definitions)
try:
	(options, files) = getopt.getopt (sys.argv[1:], sh, long)
except:
	help ()
	sys.exit (2)
	
for opt in options:	
	o = opt[0]
	a = opt[1]

	if 0:
		pass
	elif o == '--help' or o == '-h':
		help ()
	elif o == '--buid-root' or o == '-b':
		build_root = a
	elif o == '--url' or o == '-u':
		url = a
	elif o == '--verbose' or o == '-V':
		verbose_p = 1
	elif o == '--version' or o == '-v':
		identify ()
		sys.exit (0)
	elif o == '--warranty' or o == '-w':
		warranty ()
		sys.exit (0)
		
if 1:
	latest = find_latest (url)

	if os.path.isdir ('%s/%s' % (build_root, latest)):
		progress (_ ("latest is %s") % latest)
		progress (_ ("relax, %s is up to date" % package_name))
		sys.exit (0)

	get_base = url[:string.rindex (url, '/')] + '/'
	if os.path.isdir (patch_dir):
		os.chdir (patch_dir)
		if not os.path.isfile (latest + '.diff.gz'):
			get = get_base + latest + '.diff.gz'
			progress (_ ("fetching %s...") % get)
			copy_url (get, '.')

	if not os.path.isdir (build_root):
		build_root = temp_dir
	if not os.path.isdir (release_dir):
		release_dir = temp_dir
		setup_temp ()
		
	os.chdir (release_dir)
	if not os.path.isfile (latest + '.tar.gz'):
		get = get_base + latest + '.tar.gz'
		progress (_ ("fetching %s...") % get)
		copy_url (get, '.')

	build_command = './configure --prefix=$HOME/usr && make web'
	if not build (latest):
		if os.path.isdir ('%s/%s' % (build_root, package_name)):
			os.chdir ('%s/%s' % (build_root, package_name))
			previous = os.getcwd ()
			os.chdir (build_root)
			system ('rm -f %s' % package_name)
			system ('echo rm -rf %s/%s' % (build_root, previous))
			
		os.symlink ('%s/%s' % (build_root, latest),  package_name)
		
	os.chdir (original_dir)
	if release_dir != temp_dir:
		cleanup_temp ()
	sys.exit (0)
	
