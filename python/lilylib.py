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
import glob
import os
import re
import shutil
import string
import sys
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


def command_name (cmd):
	# Strip all stuf after command,
	# deal with "((latex ) >& 1 ) .." too
	cmd = re.match ('([\(\)]*)([^\\\ ]*)', cmd).group (2)
	return os.path.basename (cmd)

def error_log (name):
	name = re.sub('[^a-z]','x', name)
	return tempfile.mktemp ('%s.errorlog' % name)


def system (cmd, ignore_error = 0, progress_p = 0, be_verbose=0):
	
	'''System CMD.  If IGNORE_ERROR, do not complain when CMD
returns non zero.  If PROGRESS_P, always show progress.

RETURN VALUE

Exit status of CMD '''

	name = command_name (cmd)
	error_log_file = ''

	if be_verbose:
		progress_p = 1
		progress (_ ("Invoking `%s\'") % cmd)
	else:
		progress ( _("Running %s...") % name)

	redirect = ''
	if not progress_p:
		error_log_file = error_log (name)
		redirect = ' 1>/dev/null 2>' + error_log_file

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

def strip_extension (f, ext):
	(p, e) = os.path.splitext (f)
	if e == ext:
		e = ''
	return p + e


def search_exe_path (name):
	p = os.environ['PATH']
	exe_paths = string.split (p, ':')
	for e in exe_paths:
		full = os.path.join (e, name)
		if os.path.exists (full):
			return full
	return None


def print_environment ():
	for (k,v) in os.environ.items ():
		sys.stderr.write ("%s=\"%s\"\n" % (k, v)) 


def ps_page_count (ps_name):
	header = open (ps_name).read (1024)
	m = re.search ('\n%%Pages: ([0-9]+)', header)
	if m:
		return string.atoi (m.group (1))
	return 0

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
