################################################################
# lilylib.py -- options and stuff
# 
# source file of the GNU LilyPond music typesetter
#
# (c) 1998--2006 Han-Wen Nienhuys <hanwen@cs.uu.nl>
#                 Jan Nieuwenhuizen <janneke@gnu.org>

###  subst:\(^\|[^._a-z]\)\(abspath\|identify\|warranty\|progress\|warning\|error\|exit\|getopt_args\|option_help_str\|options_help_str\|help\|setup_temp\|read_pipe\|system\|cleanup_temp\|strip_extension\|cp_to_dir\|mkdir_p\|init\) *(
###  replace:\1ly.\2 (

### subst: \(help_summary\|keep_temp_dir_p\|option_definitions\|original_dir\|program_name\|pseudo_filter_p\|temp_dir\|verbose_p\)

import __main__
import getopt
import glob
import os
import re
import shutil
import string
import sys
import tempfile
import optparse

################################################################
# Users of python modules should include this snippet
# and customize variables below.

# We'll suffer this path init stuff as long as we don't install our
# python packages in <prefix>/lib/pythonx.y (and don't kludge around
# it as we do with teTeX on Red Hat Linux: set some environment var
# (PYTHONPATH) in profile)

# If set, LILYPONDPREFIX must take prevalence
# if datadir is not set, we're doing a build and LILYPONDPREFIX

datadir = '@local_lilypond_datadir@'
if not os.path.isdir (datadir):
	datadir = '@lilypond_datadir@'
if os.environ.has_key ('LILYPONDPREFIX') :
	datadir = os.environ['LILYPONDPREFIX']
	while datadir[-1] == os.sep:
		datadir= datadir[:-1]

sys.path.insert (0, os.path.join (datadir, 'python'))




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
	sys.stdout.write (_ ("Copyright (c) %s by") % '1998--2006')
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
	be_verbose = get_global_option('verbose_p', 'verbose')
	if be_verbose:
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
	'''Transform one option description (4-tuple) into neatly formatted string'''
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
		first = 1
		for ss in re.split ('\n\s*', s[1]):
			if first:
				str = str + '%s%s%s\n' \
					% (s[0], ' ' * (w - len (s[0]) + 3), ss)
				first = 0
			else:
				str = str + '%s%s\n' \
					% (' ' * (w + 3), ss)
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
	# Strip all stuf after command,
	# deal with "((latex ) >& 1 ) .." too
	cmd = re.match ('([\(\)]*)([^\\\ ]*)', cmd).group (2)
	return os.path.basename (cmd)

def error_log (name):
	name = re.sub('[^a-z]','x', name)
	return tempfile.mktemp ('%s.errorlog' % name)

def read_pipe (cmd, mode = 'r'):
	
	
	redirect = ''
	error_log_file = ''
	if be_verbose:
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
		
		if not be_verbose:
			contents = open (error_log_file).read ()
			if contents:
				error (_ ("The error log is as follows:"))
				sys.stderr.write (contents)

		# Ugh. code dup
		if error_log_file:
			os.unlink (error_log_file)

		exit (1)
		
	if be_verbose:
		progress ('\n')

	if error_log_file:
		os.unlink (error_log_file)
		
	return output

def get_global_option (old, new):
	try:
		return __main__.__dict__[old]
	except KeyError:
		return __main__.global_options.__dict__[new]

def system (cmd, ignore_error = 0, progress_p = 0):
	
	'''System CMD.  If IGNORE_ERROR, do not complain when CMD
returns non zero.  If PROGRESS_P, always show progress.

RETURN VALUE

Exit status of CMD '''

	name = command_name (cmd)
	error_log_file = ''

	## UGH
	be_verbose = get_global_option('verbose_p', 'verbose')
	pseudo_filter = get_global_option ('pseudo_filter_p', 'pseudo_filter')
	
	if be_verbose:
		progress_p = 1
		progress (_ ("Invoking `%s\'") % cmd)
	else:
		progress ( _("Running %s...") % name)

	redirect = ''
	if not progress_p:
		error_log_file = error_log (name)
		redirect = ' 1>/dev/null 2>' + error_log_file
	elif pseudo_filter:
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
			if be_verbose:
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
	be_verbose = get_global_option('verbose_p', 'verbose')
	if not __main__.keep_temp_dir_p:
		if be_verbose:
			progress (_ ("Cleaning %s...") % __main__.temp_dir)
		shutil.rmtree (__main__.temp_dir)
		if be_verbose:
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


def search_exe_path (name):
	p = os.environ['PATH']
	exe_paths = string.split (p, ':')
	for e in exe_paths:
		full = os.path.join (e, name)
		if os.path.exists (full):
			return full
	return None


def mkdir_p (dir, mode=0777):
	if not os.path.isdir (dir):
		makedirs (dir, mode)

def print_environment ():
	for (k,v) in os.environ.items ():
		sys.stderr.write ("%s=\"%s\"\n" % (k, v)) 


def ps_page_count (ps_name):
	header = open (ps_name).read (1024)
	m = re.search ('\n%%Pages: ([0-9]+)', header)
	if m:
		return string.atoi (m.group (1))
	return 0

def make_ps_images (ps_name, resolution = 90, papersize = "a4",
		    rename_page1_p = 0):
	base = os.path.basename (re.sub (r'\.e?ps', '', ps_name))
	header = open (ps_name).read (1024)

	png1 = base + '.png'
	pngn = base + '-page%d.png'
	output_file = pngn
	multi_page = re.search ('\n%%Pages: ', header)

	# png16m is because Lily produces color nowadays.
	if not multi_page:

		# GS can produce empty 2nd page if pngn is used.
		output_file = png1
		cmd = r'''gs\
		-dEPSCrop\
		-dGraphicsAlphaBits=4\
		-dNOPAUSE\
		-dTextAlphaBits=4\
		-sDEVICE=png16m\
		-sOutputFile='%(output_file)s'\
		-sPAPERSIZE=%(papersize)s\
		-q\
		-r%(resolution)d\
		'%(ps_name)s'\
		-c showpage\
		-c quit ''' % vars ()
	else:
		cmd = r'''gs\
		-s\
		-dGraphicsAlphaBits=4\
		-dNOPAUSE\
		-dTextAlphaBits=4\
		-sDEVICE=png16m\
		-sOutputFile='%(output_file)s'\
		-sPAPERSIZE=%(papersize)s\
		-q\
		-r%(resolution)d\
		'%(ps_name)s'\
		-c quit''' % vars ()

	remove = glob.glob (png1) + glob.glob (base + '-page*.png')
	map (os.unlink, remove)

	status = system (cmd)
	signal = 0xf & status
	exit_status = status >> 8

	if status:
		remove = glob.glob (png1) + glob.glob (base + '-page*.png')
		map (os.unlink, remove)
		error (_ ("%s exited with status: %d") % ('GS', status))
		exit (1)

	if rename_page1_p and multi_page:
		os.rename (pngn % 1, png1)
 	files = glob.glob (png1) + glob.glob (re.sub ('%d', '*', pngn))
	return files

class NonDentedHeadingFormatter (optparse.IndentedHelpFormatter):
    def format_heading(self, heading):
	    if heading:
		    return heading[0].upper() + heading[1:] + ':\n'
	    return ''
    def format_option_strings(self, option):
	    sep = ' '
	    if option._short_opts and option._long_opts:
		    sep = ','

	    metavar = ''
	    if option.takes_value():
		    metavar = '=%s' % option.metavar or option.dest.upper()

	    return "%3s%s %s%s" % (" ".join (option._short_opts),
				   sep,
				   " ".join (option._long_opts),
				   metavar)

    def format_usage(self, usage):
        return _("Usage: %s\n") % usage
    
    def format_description(self, description):
	    return description

def get_option_parser (*args, **kwargs): 
	p = optparse.OptionParser (*args, **kwargs)
	p.formatter = NonDentedHeadingFormatter () 
	return p
