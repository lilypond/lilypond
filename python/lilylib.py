###############################################################
# lilylib.py -- options and stuff
# 
# source file of the GNU LilyPond music typesetter
#
# (c) 1998--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
#                 Jan Nieuwenhuizen <janneke@gnu.org>

import __main__
import glob
import os
import re
import shutil
import sys
import optparse

################################################################
# Users of python modules should include this snippet
# and customize variables below.

# We'll suffer this path init stuff as long as we don't install our
# python packages in <prefix>/lib/pythonx.y (and don't kludge around
# it as we do with teTeX on Red Hat Linux: set some environment var
# (PYTHONPATH) in profile)

# If set, LILYPOND_DATADIR must take prevalence
# if datadir is not set, we're doing a build and LILYPOND_DATADIR

datadir = '@local_lilypond_datadir@'
if not os.path.isdir (datadir):
    datadir = '@lilypond_datadir@'
if os.environ.has_key ('LILYPOND_DATADIR') :
    datadir = os.environ['LILYPOND_DATADIR']
    while datadir[-1] == os.sep:
	datadir= datadir[:-1]

sys.path.insert (0, os.path.join (datadir, 'python'))


# Python 2.5 only accepts strings with proper Python internal encoding
# (i.e. ASCII or Unicode) when writing to stdout/stderr, so we must
# use ugettext iso gettext, and encode the string when writing to
# stdout/stderr

localedir = '@localedir@'
try:
    import gettext
    t = gettext.translation ('lilypond', localedir)
    _ = t.ugettext
except:
    def _ (s):
	return s
underscore = _

# Urg, Python 2.4 does not define stderr/stdout encoding
# Maybe guess encoding from LANG/LC_ALL/LC_CTYPE?

def encoded_write(f, s):
    f.write (s.encode (f.encoding or 'utf_8'))

# ugh, Python 2.5 optparse requires Unicode strings in some argument
# functions, and refuse them in some other places
def display_encode (s):
    return s.encode (sys.stderr.encoding or 'utf_8')

def stderr_write (s):
    encoded_write (sys.stderr, s)

progress = stderr_write

def require_python_version ():
    if sys.hexversion < 0x02040000:
        stderr_write ("Python 2.4 or newer is required to run this program.\n\
Please upgrade Python from http://python.org/download/, and if you use MacOS X,\n\
please read 'Setup for MacOS X' in Application Usage.")
        os.system ("open http://python.org/download/")
        sys.exit (2)

# Modified version of the commands.mkarg(x), which always uses 
# double quotes (since Windows can't handle the single quotes:
def mkarg(x):
    s = ' "'
    for c in x:
        if c in '\\$"`':
            s = s + '\\'
        s = s + c
    s = s + '"'
    return s

def command_name (cmd):
    # Strip all stuf after command,
    # deal with "((latex ) >& 1 ) .." too
    cmd = re.match ('([\(\)]*)([^\\\ ]*)', cmd).group (2)
    return os.path.basename (cmd)

def subprocess_system (cmd,
                       ignore_error=False,
                       progress_p=True,
                       be_verbose=False,
                       log_file=None):
    import subprocess

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

def ossystem_system (cmd,
                     ignore_error=False,
                     progress_p=True,
                     be_verbose=False,
                     log_file=None):


    name = command_name (cmd)
    if be_verbose:
	show_progress = 1
	progress (_ ("Invoking `%s\'") % cmd)
    else:
	progress ( _("Running %s...") % name)

    retval = os.system (cmd)
    if retval:
	print >>sys.stderr, 'command failed:', cmd
	if retval < 0:
	    print >>sys.stderr, "Child was terminated by signal", -retval
	elif retval > 0:
	    print >>sys.stderr, "Child returned", retval

	if ignore_error:
	    print >>sys.stderr, "Error ignored"
	else:
	    sys.exit (1)

    return abs (retval)


system = subprocess_system
if sys.platform == 'mingw32':
    
    ## subprocess x-compile doesn't work.
    system = ossystem_system

def strip_extension (f, ext):
    (p, e) = os.path.splitext (f)
    if e == ext:
	e = ''
    return p + e


def search_exe_path (name):
    p = os.environ['PATH']
    exe_paths = p.split (':')
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
        return _("Usage: %s") % usage + '\n'

    def format_description(self, description):
	return description

def get_option_parser (*args, **kwargs): 
    p = optparse.OptionParser (*args, **kwargs)
    p.formatter = NonDentedHeadingFormatter () 
    return p
