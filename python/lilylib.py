################################################################
# lilylib.py -- options and stuff
# 
# source file of the GNU LilyPond music typesetter
#
# (c)  1998--2003  Han-Wen Nienhuys <hanwen@cs.uu.nl>
#                 Jan Nieuwenhuizen <janneke@gnu.org>

###  subst:\(^\|[^._a-z]\)\(abspath\|identify\|warranty\|progress\|warning\|error\|exit\|getopt_args\|option_help_str\|options_help_str\|help\|setup_temp\|read_pipe\|system\|cleanup_temp\|strip_extension\|cp_to_dir\|mkdir_p\|init\) *(
###  replace:\1ly.\2 (

### subst: \(help_summary\|keep_temp_dir_p\|option_definitions\|original_dir\|program_name\|pseudo_filter_p\|temp_dir\|verbose_p\)

import __main__
import shutil
import string
import sys
import tempfile
import glob

################################################################
# Users of python modules should include this snippet
# and customize variables below.

# We'll suffer this path init stuff as long as we don't install our
# python packages in <prefix>/lib/pythonx.y (and don't kludge around
# it as we do with teTeX on Red Hat Linux: set some environment var
# (PYTHONPATH) in profile)

# If set, LILYPONDPREFIX must take prevalence
# if datadir is not set, we're doing a build and LILYPONDPREFIX

import getopt
import os
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
if __name__ == '__main__':
	import lilylib as ly
	global _;_=ly._
	global re;re = ly.re

	# lilylib globals
	program_name = 'unset'
	pseudo_filter_p = 0
	original_dir = os.getcwd ()
	temp_dir = os.path.join (original_dir,  '%s.dir' % program_name)
	keep_temp_dir_p = 0
	verbose_p = 0

	help_summary = _ ("lilylib module")

	option_definitions = [
		('', 'h', 'help', _ ("print this help")),
		]

	from lilylib import *
################################################################

# Handle bug in Python 1.6-2.1
#
# there are recursion limits for some patterns in Python 1.6 til 2.1. 
# fix this by importing pre instead. Fix by Mats.

if float (sys.version[0:3]) <= 2.1:
	try:
		import pre
		re = pre
		del pre
	except ImportError:
		import re
else:
	import re
	
# Attempt to fix problems with limited stack size set by Python!
# Sets unlimited stack size. Note that the resource module only
# is available on UNIX.
try:
       import resource
       resource.setrlimit (resource.RLIMIT_STACK, (-1, -1))
except:
       pass

localedir = '@localedir@'
try:
	import gettext
	gettext.bindtextdomain ('lilypond', localedir)
	gettext.textdomain ('lilypond')
	_ = gettext.gettext
except:
	def _ (s):
		return s
underscore = _

def identify (port):
	port.write ('%s (GNU LilyPond) %s\n' % (__main__.program_name, __main__.program_version))

def warranty ():
	identify (sys.stdout)
	sys.stdout.write ('\n')
	sys.stdout.write (_ ("Copyright (c) %s by") % '1998--2004')
	sys.stdout.write ('\n')
	map (lambda x: sys.stdout.write ('  %s\n' % x), __main__.copyright)
	sys.stdout.write ('\n')
	sys.stdout.write (_ ("Distributed under terms of the GNU General Public License."))
	sys.stdout.write ('\n')
	sys.stdout.write (_ ("It comes with NO WARRANTY."))
	sys.stdout.write ('\n')

def progress (s):
	sys.stderr.write (s)

def warning (s):
	sys.stderr.write (__main__.program_name + ": " + _ ("warning: %s") % s + '\n')

def error (s):
	sys.stderr.write (__main__.program_name + ": " + _ ("error: %s") % s + '\n')
	
def exit (i):
	if __main__.verbose_p:
		raise _ ('Exiting (%d)...') % i
	else:
		sys.exit (i)
		
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

	sep = '  '
	if o[1] and o[2]:
		sep = ', '
		
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
	ls = [(_ ("Usage: %s [OPTIONS]... FILE") % __main__.program_name),
	      ('\n\n'),
	      (__main__.help_summary),
	      ('\n\n'),
	      (_ ("Options:")),
	      ('\n'),
	      (options_help_str (__main__.option_definitions)),
	      ('\n\n'),
	      (_ ("Report bugs to %s.") % 'bug-lilypond@gnu.org'),
	      ('\n')]
	map (sys.stdout.write, ls)

def lilypond_version (binary):
	p = read_pipe ('%s --version ' % binary)

	ls = p.split ('\n')
	v= '<not found>'
	for l in ls:
		m = re.search ('GNU LilyPond ([0-9a-z.]+)', p)
		if m:
			v = m.group (1)
			
	return v
	
def lilypond_version_check (binary, req):
	if req[0] <> '@' :
		v = lilypond_version (binary)
		if v <> req:
			error (_("Binary %s has version %s, looking for version %s") % \
			       (binary, v, req))
			sys.exit (1)
	
	
def setup_temp ():
	
	''' Create a temporary directory, and return its name. '''
	
	if not __main__.keep_temp_dir_p:
		__main__.temp_dir = tempfile.mktemp (__main__.program_name)
	try:
		os.mkdir (__main__.temp_dir, 0700)
	except OSError:
		pass

	return __main__.temp_dir

def command_name (cmd):

	# deal with "((latex ) >& 1 ) .." too
	cmd = re.match ("([\(\)]*)([^ ]*)", cmd).group(2)
	return os.path.basename (cmd)

def error_log (name):
	name = re.sub('[^a-z]','x', name)
	return tempfile.mktemp ('%s.errorlog' % name)

def read_pipe (cmd, mode = 'r'):
	
	
	redirect = ''
	error_log_file = ''
	if __main__.verbose_p:
		progress (_ ("Opening pipe `%s\'") % cmd)
	else:
		error_log_file = error_log (command_name (cmd))
		redirect = ' 2>%s' % error_log_file
		
	pipe = os.popen (cmd + redirect, mode)
	output = pipe.read ()
	status = pipe.close ()
	# successful pipe close returns 'None'
	if not status:
		status = 0
	signal = 0x0f & status
	exit_status = status >> 8

	if status:
		error (_ ("`%s\' failed (%d)") % (cmd, exit_status))
		
		if not __main__.verbose_p:
			contents = open (error_log_file).read ()
			if contents:
				error (_ ("The error log is as follows:"))
				sys.stderr.write (contents)

		# Ugh. code dup
		if error_log_file:
			os.unlink (error_log_file)

		exit (1)
		
	if __main__.verbose_p:
		progress ('\n')

	if error_log_file:
		os.unlink (error_log_file)
		
	return output

def system (cmd, ignore_error = 0, progress_p = 0):
	
	'''System CMD.  If IGNORE_ERROR, do not complain when CMD
returns non zero.  If PROGRESS_P, always show progress.

RETURN VALUE

Exit status of CMD '''

	name = command_name (cmd)
	error_log_file = ''
	
	if __main__.verbose_p:
		progress_p = 1
		progress (_ ("Invoking `%s\'") % cmd)
	else:
		progress ( _("Running %s...") % name)

	redirect = ''
	if not progress_p:
		error_log_file = error_log (name)
		redirect = ' 1>/dev/null 2>' + error_log_file
	elif __main__.pseudo_filter_p:
		redirect = ' 1>/dev/null'

	status = os.system (cmd + redirect)
	signal = 0x0f & status
	exit_status = status >> 8
	
	if status:
		
		exit_type =  'status %d' % exit_status
		if signal:
			exit_type = 'signal %d' % signal 
		
		msg = _ ("`%s\' failed (%s)") % (name, exit_type)
		if ignore_error:
			if __main__.verbose_p:
				warning (msg + ' ' + _ ("(ignored)"))
		else:
			error (msg)
			if not progress_p and error_log_file:
				error (_ ("The error log is as follows:"))
				sys.stderr.write (open (error_log_file).read ())
			if error_log_file:
				os.unlink (error_log_file)
			exit (1)

	if error_log_file:
		os.unlink (error_log_file)
	progress ('\n')
	return status

def cleanup_temp ():
	if not __main__.keep_temp_dir_p:
		if __main__.verbose_p:
			progress (_ ("Cleaning %s...") % __main__.temp_dir)
		shutil.rmtree (__main__.temp_dir)
		if __main__.verbose_p:
			progress ('\n')


def strip_extension (f, ext):
	(p, e) = os.path.splitext (f)
	if e == ext:
		e = ''
	return p + e


def cp_to_dir (pattern, dir):
	"Copy files matching re PATTERN from cwd to DIR"
	
	# Duh.  Python style portable: cp *.EXT OUTDIR
	# system ('cp *.%s %s' % (ext, outdir), 1)

	files = filter (lambda x, p=pattern: re.match (p, x), os.listdir ('.'))
	map (lambda x, d=dir: shutil.copy2 (x, os.path.join (d, x)), files)


# Python < 1.5.2 compatibility
#
# On most platforms, this is equivalent to
#`normpath(join(os.getcwd()), PATH)'.  *Added in Python version 1.5.2*

if os.path.__dict__.has_key ('abspath'):
	abspath = os.path.abspath
else:
	def abspath (path):
		return os.path.normpath (os.path.join (os.getcwd (), path))

if os.__dict__.has_key ('makedirs'):
	makedirs = os.makedirs
else:
	def makedirs (dir, mode=0777):
		system ('mkdir -p %s' % dir)


def mkdir_p (dir, mode=0777):
	if not os.path.isdir (dir):
		makedirs (dir, mode)


environment = {}

# tex needs lots of memory, more than it gets by default on Debian
non_path_environment = {
	'extra_mem_top' : '1000000',
	'extra_mem_bottom' : '1000000',
	'pool_size' : '250000',
}

def setup_environment ():
	global environment

	kpse = read_pipe ('kpsexpand \$TEXMF')
	texmf = re.sub ('[ \t\n]+$','', kpse)
	type1_paths = read_pipe ('kpsewhich -expand-path=\$T1FONTS')
	
	environment = {
		# TODO: * prevent multiple addition.
		#       * clean TEXINPUTS, MFINPUTS, TFMFONTS,
		#         as these take prevalence over $TEXMF
		#         and thus may break tex run?
		
		'TEXMF' : "{%s,%s}" % (datadir, texmf) ,
		'GS_FONTPATH' : type1_paths,
		'GS_LIB' : datadir + '/ps',
		}
	
	# $TEXMF is special, previous value is already taken care of
	if os.environ.has_key ('TEXMF'):
		del os.environ['TEXMF']
 
	for key in environment.keys ():
		val = environment[key]
		if os.environ.has_key (key):
			val = os.environ[key] + os.pathsep + val 
		os.environ[key] = val

	for key in non_path_environment.keys ():
		val = non_path_environment[key]
		os.environ[key] = val

def print_environment ():
	for (k,v) in os.environ.items ():
		sys.stderr.write ("%s=\"%s\"\n" % (k, v)) 

BOUNDING_BOX_RE = '^%%BoundingBox: (-?[0-9]+) (-?[0-9]+) (-?[0-9]+) (-?[0-9]+)'
def get_bbox (filename):
	bbox = filename + '.bbox'
	## -sOutputFile does not work with bbox?
	cmd = 'gs -sDEVICE=bbox -q -dNOPAUSE %s -c quit 2>%s' % \
	      (filename, bbox)
	system (cmd, progress_p = 1)
	box = open (bbox).read ()
	m = re.match (BOUNDING_BOX_RE, box)
	gr = []
	if m:
		gr = map (string.atoi, m.groups ())
	
	return gr


def make_ps_images (ps_name, resolution = 90):
	## todo:
	## have better algorithm for deciding when to crop page,
	## and when to show full page
	base = re.sub (r'\.e?ps', '', ps_name)
	
	header = open (ps_name).read (1024)

	match = re.match (BOUNDING_BOX_RE, header)
	bbox = []
	if match:
		bbox = map (string.atoi, match.groups ())

	multi_page = re.search ('\n%%Pages: ', header)
	cmd = ''

	if multi_page == None:

		if bbox == []:
			bbox = get_bbox (ps_name)
			
		trans_ps = ps_name + '.trans.ps'
		output_file = re.sub (r'\.e?ps', '.png', ps_name)

		# need to have margin, otherwise edges of letters will
		# be cropped off.

		margin = 3 
		fo = open (trans_ps, 'w')
		fo.write ('%d %d translate\n' % (-bbox[0] + margin,
						 -bbox[1] + margin))
		fo.close ()

		x = (2* margin + bbox[2] - bbox[0]) \
		    * resolution / 72.0
		y = (2* margin + bbox[3] - bbox[1]) \
		    * resolution / 72.0
		if x == 0:
			x = 1
		if y == 0:
			y = 1

		cmd = r'''gs -g%dx%d -sDEVICE=pnggray  -dTextAlphaBits=4 -dGraphicsAlphaBits=4  -q -sOutputFile=%s -r%d -dNOPAUSE %s %s -c quit ''' % \
		      (x, y, output_file, resolution, trans_ps, ps_name)

		rms = glob.glob (base + '-page*.png')
		map (os.unlink, rms)
	else:
		output_file = re.sub (r'\.e?ps', '-page%d.png', ps_name)

		rmfile = base + '.png'
		if os.path.isfile (rmfile):
			os.unlink (rmfile)
		
		cmd = r'''gs -s  -sDEVICE=pnggray  -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -q -sOutputFile=%s -dNOPAUSE -r%d %s -c quit''' % (output_file,
																      resolution, ps_name)

	status = system (cmd)
	signal = 0xf & status
	exit_status = status >> 8

	
	if status:
		os.unlink (png)
		error (_ ("Removing output file"))
		exit (1)


	cmd = r'''gs -s  -sDEVICE=pnggray  -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -q -sOutputFile=%s -dNOPAUSE -r%d %s -c quit''' % (output_file,
																      resolution, ps_name)

	return output_file

