# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 1998--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
import codecs
import gettext
import optparse
import os
import sys

sys.stdin = codecs.getreader('utf-8')(sys.stdin.detach())
sys.stdout = codecs.getwriter('utf-8')(sys.stdout.detach())
sys.stderr = codecs.getwriter('utf-8')(sys.stderr.detach())

# Lilylib globals.
program_name = os.path.basename(sys.argv[0])

# Logging framework: We have the following output functions:
#    error
#    warning
#    progress
#    debug

# TODO: use the standard logging module
_loglevels = {"NONE": 0, "ERROR": 1, "WARN": 2,
              "BASIC": 3, "PROGRESS": 4, "INFO": 5, "DEBUG": 6}

_loglevel = _loglevels["PROGRESS"]


def set_loglevel(l):
    global _loglevel
    newlevel = _loglevels.get(l, -1)
    if newlevel >= 0:
        debug_output(_("Setting loglevel to %s") % l)
        _loglevel = newlevel
    else:
        error(_("Unknown or invalid loglevel '%s'") % l)


def handle_loglevel_option(option, opt_str, value, parser, *args):
    if value:
        set_loglevel(value)
    elif args:
        set_loglevel(args[0])


def _is_loglevel(l):
    global _loglevel
    return _loglevel >= _loglevels[l]


def is_verbose():
    return _is_loglevel("DEBUG")


def _print_logmessage(level, s, fullmessage=True, newline=True):
    if _is_loglevel(level):
        if fullmessage:
            s = program_name + ": " + s + "\n"
        elif newline:
            s += '\n'
        sys.stderr.write(s)
        sys.stderr.flush()


def error(s):
    _print_logmessage("ERROR", _("error: %s") % s)


def warning(s):
    _print_logmessage("WARN", _("warning: %s") % s)


def progress(s, fullmessage=False, newline=True):
    _print_logmessage("PROGRESS", s, fullmessage, newline)


def debug_output(s, fullmessage=False, newline=True):
    _print_logmessage("DEBUG", s, fullmessage, newline)


def handle_globs(args):
    # On Windows, we have to expand globs by ourselves.
    if os.name == 'nt':
        from glob import glob
        files = []
        for arg in args:
            # In `cmd.exe`, only `*` and `?` are metacharacters but not `[`
            # (and `]`).
            expanded = glob(arg.replace('[', '[[]'))
            if expanded:
                files.extend(expanded)
            else:
                # If the expansion returns nothing, add the argument as-is;
                # more handling is then to be done by the caller.
                files.append(arg)
        return files
    else:
        return args


class _NonDentedHeadingFormatter (optparse.IndentedHelpFormatter):
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

        return "%2s%s %s%s" % (", ".join(option._short_opts),
                               sep,
                               ", ".join(option._long_opts),
                               metavar)

    # Only use one level of indentation (even for groups and nested groups),
    # since we don't indent the headings, either
    def indent(self):
        self.current_indent = self.indent_increment
        self.level += 1

    def dedent(self):
        self.level -= 1
        if self.level <= 0:
            self.current_indent = ''
            self.level = 0

    def format_usage(self, usage):
        return _("Usage: %s") % usage + '\n'

    def format_description(self, description):
        return description


def get_option_parser(*args, **kwargs):
    p = optparse.OptionParser(*args, **kwargs)
    p.formatter = _NonDentedHeadingFormatter()
    p.formatter.set_parser(p)
    return p

def brace_matcher(n):
    # poor man's matched brace scanning, gives up
    # after n+1 levels.  Matches any string with balanced
    # braces inside; add the outer braces yourself if needed.
    # Nongreedy.
    return r"[^{}]*?(?:{"*n+r"[^{}]*?"+r"}[^{}]*?)*?"*n

def paren_matcher(n):
    """Same as brace_matcher, for parentheses."""
    return r"[^()]*?(?:\("*n+r"[^()]*?"+r"\)[^()]*?)*?"*n
