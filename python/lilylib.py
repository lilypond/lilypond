###############################################################
# lilylib.py -- options and stuff
# 
# source file of the GNU LilyPond music typesetter
#
# (c) 1998--2006 Han-Wen Nienhuys <hanwen@cs.uu.nl>
#                 Jan Nieuwenhuizen <janneke@gnu.org>

import __main__
import glob
import os
import re
import shutil
import string
import sys
import optparse
import subprocess

## windows mingw cross compile doesn't have selectmodule.so
have_fcntl = True
try:
    import fcntl
except ImportError:
    have_fcntl = False

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
progress = sys.stderr.write 


def command_name (cmd):
    # Strip all stuf after command,
    # deal with "((latex ) >& 1 ) .." too
    cmd = re.match ('([\(\)]*)([^\\\ ]*)', cmd).group (2)
    return os.path.basename (cmd)

def system (cmd,
	      ignore_error=False,
	      progress_p=True,
	      be_verbose=False,
	      log_file=None):

    show_progress= progress_p 
    name = command_name (cmd)
    error_log_file = ''

    if be_verbose:
	show_progress = 1
	progress (_ ("Invoking `%s\'") % cmd)
    else:
	progress ( _("Running %s...") % name)


    stdout_setting = None
    if not show_progress:
	stdout_setting = subprocess.PIPE

    proc = subprocess.Popen (cmd,
			     shell=True,
			     universal_newlines=True,
			     stdout=stdout_setting,
			     stderr=stdout_setting)

    log = ''

    if show_progress:
	retval = proc.wait()
    else:
	log = proc.communicate ()
	retval = proc.returncode


    if retval:
	print >>sys.stderr, 'command failed:', cmd
	if retval < 0:
	    print >>sys.stderr, "Child was terminated by signal", -retval
	elif retval > 0:
	    print >>sys.stderr, "Child returned", retval

	if ignore_error:
	    print >>sys.stderr, "Error ignored"
	else:
	    if not show_progress:
		print log[0]
		print log[1]
	    sys.exit (1)

    return abs (retval)

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
