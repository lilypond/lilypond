# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 1998--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>
#                Jan Nieuwenhuizen <janneke@gnu.org>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

import __main__
import glob
import os
import re
import shutil
import sys
import optparse
import time

################################################################
# Users of python modules should include this snippet
# and customize variables below.


# Python 2.5 only accepts strings with proper Python internal encoding
# (i.e. ASCII or Unicode) when writing to stdout/stderr, so we must
# use ugettext iso gettext, and encode the string when writing to
# stdout/stderr

localedir = '@localedir@'
try:
    import gettext
    t = gettext.translation ('lilypond', localedir)
    _ = t.ugettext
    ungettext = t.ungettext
except:
    def _ (s):
        return s
    def ungettext (s, p, n):
        if n == 1:
            return s
        return p
underscore = _

# Urg, Python 2.4 does not define stderr/stdout encoding
# Maybe guess encoding from LANG/LC_ALL/LC_CTYPE?

reload (sys)
sys.setdefaultencoding ('utf-8')
import codecs
sys.stdout = codecs.getwriter ('utf8') (sys.stdout)
sys.stderr = codecs.getwriter ('utf8') (sys.stderr)

def encoded_write(f, s):
    f.write (s.encode (f.encoding or 'utf-8', 'replace'))

# ugh, Python 2.5 optparse requires Unicode strings in some argument
# functions, and refuse them in some other places
def display_encode (s):
    return s.encode (sys.stderr.encoding or 'utf-8', 'replace')

# Lilylib globals.
program_version = '@TOPLEVEL_VERSION@'
program_name = os.path.basename (sys.argv[0])


# Check if program_version contains @ characters. This will be the case if
# the .py file is called directly while building the lilypond documentation.
# If so, try to check for the env var LILYPOND_VERSION, which is set by our
# makefiles and use its value.
at_re = re.compile (r'@')
if at_re.match (program_version):
    if os.environ.has_key('LILYPOND_VERSION'):
        program_version = os.environ['LILYPOND_VERSION']
    else:
        program_version = "unknown"


# Logging framework: We have the following output functions:
#    error
#    warning
#    progress
#    debug

loglevels = {"NONE":0, "ERROR":1, "WARN":2, "BASIC":3, "PROGRESS":4, "INFO":5, "DEBUG":6}

loglevel = loglevels["PROGRESS"]

def set_loglevel (l):
    global loglevel
    newlevel = loglevels.get (l, -1)
    if newlevel > 0:
        debug_output (_ ("Setting loglevel to %s") % l)
        loglevel = newlevel
    else:
        error (_ ("Unknown or invalid loglevel '%s'") % l)


def handle_loglevel_option (option, opt_str, value, parser, *args):
    if value:
        set_loglevel (value);
    elif args:
        set_loglevel (args[0]);

def is_loglevel (l):
    global loglevel
    return loglevel >= loglevels[l];

def is_verbose ():
    return is_loglevel ("DEBUG")

def stderr_write (s):
    encoded_write (sys.stderr, s)

def print_logmessage (level, s, fullmessage = True, newline = True):
    if (is_loglevel (level)):
        if fullmessage:
            stderr_write (program_name + ": " + s + '\n')
        elif newline:
            stderr_write (s + '\n')
        else:
            stderr_write (s)

def error (s):
    print_logmessage ("ERROR", _ ("error: %s") % s);

def warning (s):
    print_logmessage ("WARN", _ ("warning: %s") % s);

def basic_progress (s):
    print_logmessage ("BASIC", s);

def progress (s, fullmessage = False, newline = True):
    print_logmessage ("PROGRESS", s, fullmessage, newline);

def debug_output (s, fullmessage = False, newline = True):
    print_logmessage ("DEBUG", s, fullmessage, newline);



def require_python_version ():
    if sys.hexversion < 0x02040000:
        error ("Python 2.4 or newer is required to run this program.\n\
Please upgrade Python from http://python.org/download/, and if you use MacOS X,\n\
please read 'Setup for MacOS X' in Application Usage.")
        os.system ("open http://python.org/download/")
        sys.exit (2)

# A modified version of the commands.mkarg(x) that always uses
# double quotes (since Windows can't handle the single quotes)
# and escapes the characters \, $, ", and ` for unix shells.
def mkarg(x):
    if os.name == 'nt':
        return ' "%s"' % x
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
                       redirect_output=False,
                       log_file=None):
    import subprocess

    show_progress= progress_p
    name = command_name (cmd)
    error_log_file = ''

    if redirect_output:
        progress (_ ("Processing %s.ly") % log_file)
    else:
        if be_verbose:
            show_progress = 1
            progress (_ ("Invoking `%s\'") % cmd)
        else:
            progress ( _("Running %s...") % name)

    stdout_setting = None
    stderr_setting = None
    if not show_progress:
        stdout_setting = subprocess.PIPE

    if redirect_output:
        stderr_filename = log_file + '.log'
        stderr_setting = open(stderr_filename, 'w')

    proc = subprocess.Popen (cmd,
                             shell=True,
                             universal_newlines=True,
                             stdout=stdout_setting,
                             stderr=stderr_setting)

    log = ''

    if redirect_output:
        while proc.poll()==None:
            time.sleep(0.01)
        retval = proc.returncode
        stderr_setting.close()
    else:
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
            print >>sys.stderr, "Error ignored by lilylib"
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
                     redirect_output=False,
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

    # Only use one level of indentation (even for groups and nested groups),
    # since we don't indent the headeings, either
    def indent(self):
        self.current_indent = self.indent_increment
        self.level += 1
    def dedent(self):
        self.level -= 1
        if self.level <= 0:
            self.current_indent = ''
            self.level = 0;

    def format_usage(self, usage):
        return _("Usage: %s") % usage + '\n'

    def format_description(self, description):
        return description

class NonEmptyOptionParser (optparse.OptionParser):
    "A subclass of OptionParser that gobbles empty string arguments."

    def parse_args (self, args=None, values=None):
        options, args = optparse.OptionParser.parse_args (self, args, values)
        return options, filter (None, args)

def get_option_parser (*args, **kwargs):
    p = NonEmptyOptionParser (*args, **kwargs)
    p.formatter = NonDentedHeadingFormatter ()
    p.formatter.set_parser (p)
    return p
