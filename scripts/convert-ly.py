#!@TARGET_PYTHON@

# convert-ly.py -- Update old LilyPond input files (fix name?)
# converting rules are found in python/convertrules.py

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 1998--2015  Han-Wen Nienhuys <hanwen@xs4all.nl>
#                 Jan Nieuwenhuizen <janneke@gnu.org>
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

import os
import sys
import re
import shutil

"""
@relocate-preamble@
"""

import lilylib as ly
global _;_=ly._

ly.require_python_version ()

import convertrules

lilypond_version_re_str = '\\\\version *\"([0-9.]+)"'
lilypond_version_re = re.compile (lilypond_version_re_str)

lilypond_version_strict_re_str = '\\\\version *\"([0-9]+[.][0-9]+[.][0-9]+)"'
lilypond_version_strict_re = re.compile (lilypond_version_strict_re_str)

help_summary = (
_ ('''Update LilyPond input to newer version.  By default, update from the
version taken from the \\version command, to the current LilyPond version.''')
+ "\n"
+ _ ("If FILE is `-', read from standard input.")
+ "\n\n"
+ _ ("Examples:")
+ '''
  $ convert-ly -e old.ly
  $ convert-ly --from=2.3.28 --to=2.5.21 foobar.ly > foobar-new.ly
''')

copyright = ('Jan Nieuwenhuizen <janneke@gnu.org>',
             'Han-Wen Nienhuys <hanwen@xs4all.nl>')

program_name = os.path.basename (sys.argv[0])
program_version = '@TOPLEVEL_VERSION@'

authors = ('Jan Nieuwenhuizen <janneke@gnu.org>',
           'Han-Wen Nienhuys <hanwen@xs4all.nl>')

def identify ():
    ly.progress ('%s (GNU LilyPond) %s\n' % (program_name, program_version))

def warranty ():
    identify ()
    ly.encoded_write (sys.stdout, '''
%s

%s

%s
%s
''' % ( _ ('Copyright (c) %s by') % '2001--2015',
        ' '.join (authors),
        _ ('Distributed under terms of the GNU General Public License.'),
        _ ('It comes with NO WARRANTY.')))

def get_option_parser ():
    p = ly.get_option_parser (usage=_ ("%s [OPTION]... FILE") % 'convert-ly',
                  description=help_summary,
                  add_help_option=False)

    p.version="@TOPLEVEL_VERSION@"
    p.add_option("--version",
                 action="version",
                 help=_ ("show version number and exit"))

    p.add_option("-h", "--help",
                 action="help",
                 help=_ ("show this help and exit"))

    p.add_option ('-f', '--from', 
              action="store",
              metavar=_ ("VERSION"),
              dest="from_version",
              help=_ ("start from VERSION [default: \\version found in file]"),
              default='')
    
    p.add_option ('-e', '--edit', help=_ ("edit in place"),
              action='store_true')

    p.add_option ("-l", "--loglevel",
                  help=_ ("Print log messages according to LOGLEVEL "
                          "(NONE, ERROR, WARNING, PROGRESS (default), DEBUG)"),
                  metavar=_ ("LOGLEVEL"),
                  action='callback',
                  callback=ly.handle_loglevel_option,
                  type='string')

    p.add_option ('-n', '--no-version',
              help=_ ("do not add \\version command if missing"),
              action='store_true',
              dest='skip_version_add',
              default=False)

    p.add_option ('-c', '--current-version',
              help=_ ("force updating \\version number to %s") % program_version,
              action='store_true',
              dest='force_current_version',
              default=False)

    p.add_option ('-d', '--diff-version-update',
              help=_ ("only update \\version number if file is modified"),
              action='store_true',
              dest='diff_version_update',
              default=False)

    p.add_option ("-s", '--show-rules',
              help=_ ("show rules [default: -f 0, -t %s]") % program_version,
              dest='show_rules',
              action='store_true', default=False)

    p.add_option ('-t', '--to',
              help=_ ("convert to VERSION [default: %s]") % program_version,
              metavar=_ ('VERSION'),
              action='store',
              dest="to_version",
              default='')

    p.add_option ('-b', '--backup-numbered',
              help=_ ("make a numbered backup [default: filename.ext~]"),
              action='store_true',
              dest="backup_numbered",
              default='')

    p.add_option ('-w', '--warranty', help=_ ("show warranty and copyright"),
           action='store_true',
           ),
    p.add_option_group ('',
                        description=(
            _ ("Report bugs via %s")
            % 'http://post.gmane.org/post.php'
            '?group=gmane.comp.gnu.lilypond.bugs') + '\n')
    
    return p

def str_to_tuple (s):
    return tuple ([int(n) for n in s.split ('.')])

def tup_to_str (t):
    return '.'.join (['%s' % x for x in t])

def version_cmp (t1, t2):
    for x in [0, 1, 2]:
        if t1[x] - t2[x]:
            return t1[x] - t2[x]
    return 0

def get_conversions (from_version, to_version):
    def is_applicable (v, f = from_version, t = to_version):
        return version_cmp (v[0], f) > 0 and version_cmp (v[0], t) <= 0
    return filter (is_applicable, convertrules.conversions)

def latest_version ():
    return convertrules.conversions[-1][0]

def show_rules (file, from_version, to_version):
    for x in convertrules.conversions:
        if (not from_version or x[0] > from_version) \
           and (not to_version or x[0] <= to_version):
            ly.encoded_write  (file, '%s: %s\n' % (tup_to_str (x[0]), x[2]))

def do_conversion (str, from_version, to_version):
    """Apply conversions from FROM_VERSION to TO_VERSION.  Return
tuple (LAST,LASTCHANGED,STR,ERRORS), with the last applied conversion,
the last conversion resulting in a change, the resulting
string and the number of errors."""
    conv_list = get_conversions (from_version, to_version)

    ly.progress (_ ("Applying conversion: "), newline = False)

    last_conversion = None
    last_change = None
    errors = 0
    try:
        for x in conv_list:
            if x != conv_list[-1]:
                ly.progress (tup_to_str (x[0]), newline = False)
                ly.progress (', ', newline = False)
            else:
                ly.progress (tup_to_str (x[0]))
            newstr = x[1] (str)
            last_conversion = x[0]
            if (newstr != str):
                last_change = last_conversion
            str = newstr

    except convertrules.FatalConversionError:
        ly.error (_ ("Error while converting")
                  + '\n'
                  + _ ("Stopping at last successful rule"))
        errors += 1

    return (last_conversion, last_change, str, errors)

def guess_lilypond_version (input):
    m = lilypond_version_strict_re.search (input)
    if m:
        return m.group (1)
    m = lilypond_version_re.search (input)
    if m:
        raise InvalidVersion (m.group (1))
    else:
        return ''

class FatalConversionError (Exception):
    pass

class UnknownVersion (Exception):
    pass

class InvalidVersion (Exception):
    def __init__ (self, version):
      self.version = version

def back_up (file, numbered):
    if numbered:
      n = 0
      while True:
        n = n + 1
        back_up = file + '.~' + str(n) + '~'
        if not os.path.exists (back_up):
            break
    else:
      back_up = file + '~'
    shutil.copy2 (file, back_up)
    return back_up

def do_one_file (infile_name):
    ly.progress (_ (u"Processing `%s\'... ") % infile_name, True)

    if infile_name:
        infile = open (infile_name, 'r')
        input = infile.read ()
        infile.close ()
    else:
        input = sys.stdin.read ()

    from_version = None
    to_version = None
    if global_options.from_version:
        from_version = global_options.from_version
    else:
        guess = guess_lilypond_version (input)
        if not guess:
            raise UnknownVersion ()
        from_version = str_to_tuple (guess)

    if global_options.to_version:
        to_version = global_options.to_version
    else:
        to_version = latest_version ()

    if len (from_version) != 3:
        raise InvalidVersion (".".join ([str(n) for n in from_version]))


    (last, last_change, result, errors) = \
        do_conversion (input, from_version, to_version)

    if global_options.force_current_version and \
            (last is None or last == to_version):
        last = str_to_tuple (program_version)
    if last:
        if global_options.diff_version_update:
            # Note that last_change can be set even if the result is
            # the same if two conversion rules cancelled out
            if result == input:
                # make no (actual) change to the version number
                last = from_version
            else:
                last = last_change
                # If the last update was to an unstable version
                # number, and the final update target is no longer in
                # the same unstable series, we update to the stable
                # series following the unstable version.
                if last[1]%2: # unstable
                    next_stable = (last[0], last[1]+1, 0)
                    if next_stable <= to_version:
                        last = next_stable

        newversion = r'\version "%s"' % tup_to_str (last)
        if lilypond_version_re.search (result):
            result = re.sub (lilypond_version_re_str,
                     '\\' + newversion, result)
        elif not global_options.skip_version_add:
            result = newversion + '\n' + result

    ly.progress ('\n')

    if global_options.edit:
        backup = back_up (infile_name, global_options.backup_numbered)
        outfile = open (infile_name, 'w')
    else:
        outfile = sys.stdout

    outfile.write (result)
    
    sys.stderr.flush ()

    return errors

def do_options ():
    opt_parser = get_option_parser()
    (options, args) = opt_parser.parse_args ()

    if options.warranty:
        warranty ()
        sys.exit (0)

    if options.from_version:
        options.from_version = str_to_tuple (options.from_version)
    if options.to_version:
        options.to_version = str_to_tuple (options.to_version)

    options.outfile_name = ''
    global global_options
    global_options = options

    if not args and not options.show_rules:
        opt_parser.print_help ()
        sys.exit (2)

    return args

def main ():
    files = do_options ()

    # should parse files[] to read \version?
    if global_options.show_rules:
        show_rules (sys.stdout, global_options.from_version, global_options.to_version)
        sys.exit (0)

    identify ()

    errors = 0
    for f in files:
        f = f.decode (sys.stdin.encoding or "utf-8")
        if f == '-':
            f = ''
        elif not os.path.isfile (f):
            ly.error (_ (u"%s: Unable to open file") % f)
            errors += 1
            continue
        try:
            errors += do_one_file (f)
        except UnknownVersion:
            ly.error (_ (u"%s: Unable to determine version.  Skipping") % f)
            errors += 1
        except InvalidVersion:
            # Compat code for 2.x and 3.0 syntax ("except .. as v" doesn't 
            # work in python 2.4!):
            t, v, b = sys.exc_info ()
            ly.error (_ (u"%s: Invalid version string `%s' \n"
                         "Valid version strings consist of three numbers, "
                         "separated by dots, e.g. `2.8.12'") % (f, v.version) )
            errors += 1

    if errors:
        ly.warning (ly.ungettext ("There was %d error.",
            "There were %d errors.", errors) % errors)
        sys.exit (1)

main ()
