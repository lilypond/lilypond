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

    * use urllib iso ftplib

    * more flexible build/ftp/patches/releases paths

    
    show only: --command='echo "latest is: %n-%v"'
'''

import ftplib
import fnmatch
import getopt
import re
import operator
import os
import tempfile
import stat
import string
import sys
import __main__


url = 'file:/home/ftp/pub/gnu/LilyPond/development/lilypond-*.tar.gz'
url = 'ftp://appel.lilypond.org/pub/gnu/LilyPond/development/lilypond-*.tar.gz'
url = 'ftp://ftp.cs.uu.nl/pub/GNU/LilyPond/development/lilypond-*.tar.gz'

remove_previous_p = 0


sys.path.append ('@datadir@/python')
sys.path.append ('@datadir@/buildscripts/out')

try:
	import gettext
	gettext.bindtextdomain ('lilypond', '@localedir@')
	gettext.textdomain ('lilypond')
	_ = gettext.gettext
except:
	def _ (s):
		return s

# Attempt to fix problems with limited stack size set by Python!
# Sets unlimited stack size. Note that the resource module only
# is available on UNIX.
try:
       import resource
       resource.setrlimit (resource.RLIMIT_STACK, (-1, -1))
except:
       pass


program_name = 'build-lily'
package_name = 'lilypond'
help_summary = _ ("Fetch and rebuild from latest source package")

build_root = os.path.join (os.environ ['HOME'], 'usr', 'src')
release_dir = build_root + '/releases'
patch_dir = build_root + '/patches'
original_dir = os.getcwd ()
temp_dir = os.path.join (original_dir,  '%s.dir' % program_name)


option_definitions = [
	('DIR', 'b', 'build-root', _ ("unpack and build in DIR [%s]") % build_root),
	('COMMAND', 'c', 'command', _ ("execute COMMAND, subtitute:") \
	 + '\n                            ' + _ ("%b: build root") \
	 + '\n                            ' + _ ("%n: package name") \
	 + '\n                            ' + _ ("%r: release directory") \
	 + '\n                            ' + _ ("%t: tarball") \
	 + '\n                            ' + _ ("%v: package version") \
	 ),
	('', 'h', 'help', _ ("this help")),
        ('', 'k', 'keep', _ ("keep all output, and name the directory %s") % temp_dir),
        ('EMAIL', 'n', 'notify', _ ("upon failure notify EMAIL[,EMAIL]")),
	('', 'r', 'remove-previous', _ ("remove previous build")),
	('', 'V', 'verbose', _ ("verbose")),
	('', 'v', 'version', _ ("print version number")),
	('URL', 'u', 'url', _ ("fetch and build URL [%s]") % url),
	('', 'w', 'warranty', _ ("show warranty and copyright")),
	]

from lilylib import *


notify = 0

build_command = '''
set -x
cd %b &&
[ -d %n-%v ] && exit 1 || true;
mkdir -p %n-%v
(
tar xzf %r/%t &&
rm -f building &&
ln -s %n-%v building &&
cd %n-%v &&
./configure --prefix=$HOME/usr && make all web
) >> %n-%v/log.txt 2>&1 &&
rm -f %n &&
ln -s %n-%v %n
'''


def list_file (user, passwd, host, dir, file):
	match = []
	for i in os.listdir (dir):
		if fnmatch.fnmatch (i, file):
			match.append (i)
	return match

list_ = list_file

def list_ftp (user, passwd, host, dir, file):
	if user == 'None':
		user = 'anonymous'
	if passwd == 'None':
		passwd = program_name

	ftp = ftplib.FTP (host)
	ftp.login (user, passwd)
	ftp.set_pasv (1)
	ftp.cwd (dir)
	list = ftp.nlst (file)
	try:
		ftp.quit ()
	except:
		ftp.close ()
	return list
	
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

	ftp = ftplib.FTP (host)
	ftp.login (user, passwd)
	ftp.set_pasv (1)
	t = tempfile.mktemp (program_name)
	try:
		f = open (t, 'w')
		ftp.retrbinary ('RETR %s/%s' % (dir, file),
			lambda x, f=f: f.write (x))
		f.close ()
		# huh? Invalid cross-device link
		# os.rename (t, file)
		system ('mv %s %s' % (t, file))
	except:
		os.remove (t)
		raise 'Foo'
	try:
		ftp.quit ()
	except:
		ftp.close ()
	return list
	


def copy_url (url, dir):
	os.chdir (dir)
	s = "copy_%s ('%s', '%s', '%s', '%s', '%s')" % split_url (url)
	eval (s)


def find_latest (url):
	progress (_ ("Listing `%s'...") % url)
	list = map (split_package, list_url (url))
	list.sort ()
	return join_package (list[-1])

def build (p):
	tar_ball = p + '.tar.gz'
	(tar_name, tar_version) = split_package (tar_ball)
	
	expand = {
		'%b' : build_root,
		'%n' : tar_name,
		'%r' : release_dir,
		'%v' : version_tuple_to_str (tar_version),
		'%t' : tar_ball,
		}

	c = build_command
	for i in expand.keys ():
		c = re.sub (i, expand[i], c)
	return system (c, 1)



(sh, long) = getopt_args (__main__.option_definitions)
try:
	(options, files) = getopt.getopt(sys.argv[1:], sh, long)
except getopt.error, s:
	errorport.write ('\n')
	errorport.write (_ ("error: ") + _ ("getopt says: `%s\'" % s))
	errorport.write ('\n')
	errorport.write ('\n')
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
	elif o == '--command' or o == '-c':
		build_command = a
	elif o == '--notify' or o == '-n':
		notify = a
	elif o == '--remove-previous' or o == '-r':
		remove_previous_p = 1
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

	# if os.path.isdir ('%s/%s' % (build_root, latest)):
	if os.path.exists ('%s/%s/index.html' % (build_root, latest)):
		progress (_ ("latest is: %s") % latest)
		progress (_ ("relax, %s is up to date" % package_name))
		sys.exit (0)

	get_base = url[:string.rindex (url, '/')] + '/'
	if os.path.isdir (patch_dir):
		os.chdir (patch_dir)
		if not os.path.isfile (latest + '.diff.gz'):
			get = get_base + latest + '.diff.gz'
			progress (_ ("Fetching `%s'...") % get)
			copy_url (get, '.')

	if not os.path.isdir (build_root):
		build_root = temp_dir
		
	if not os.path.isdir (release_dir):
		release_dir = temp_dir
		setup_temp ()
		
	os.chdir (release_dir)
	if not os.path.isfile (latest + '.tar.gz'):
		get = get_base + latest + '.tar.gz'
		progress (_ ("Fetching `%s'...") % get)
		copy_url (get, '.')

	if os.path.isdir (os.path.join (build_root, package_name)):
		os.chdir (os.path.join (build_root, package_name))
		previous = os.getcwd ()
	else:
		previous = 0

	progress (_ ("Building `%s'...") % latest)
	os.chdir (build_root)
	if not build (latest):
		if previous and remove_previous_p:
			system ('rm -rf %s' % os.path.join (build_root, previous))
	else:
		if notify:
			system ('(date; uname -a) | mail -s "%s failed" %s' % (program_name, notify))
		sys.exit (1)
		
	os.chdir (original_dir)
	if release_dir != temp_dir:
		cleanup_temp ()
	sys.exit (0)
	
