#!@TARGET_PYTHON@

# convert-ly.py -- Update old LilyPond input files (fix name?)
# converting rules are found in python/convertrules.py

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 1998--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
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

import gettext
import io
import os
import re
import shutil
import sys

"""
@relocate-preamble@
"""

# Load translation and install _() into Python's builtins namespace.
gettext.install('lilypond', '@localedir@')

import convertrules
import lilylib as ly

lilypond_version_re_str = '\\\\version *\"([0-9.]+)"'
lilypond_version_re = re.compile(lilypond_version_re_str)

lilypond_version_strict_re_str = '\\\\version *\"([0-9]+(?:[.]([0-9]+))([.][0-9]+)?)"'
lilypond_version_strict_re = re.compile(lilypond_version_strict_re_str)

help_summary = (
    _('''Update LilyPond input to newer version.  By default, update from the
version taken from the \\version command, to the current LilyPond version.''')
    + "\n"
    + _("If FILE is `-', read from standard input.")
    + "\n\n"
    + _("Examples:")
    + '''
  $ convert-ly -e old.ly
  $ convert-ly --from=2.3.28 --to=2.5.21 foobar.ly > foobar-new.ly
''')

copyright = ('Jan Nieuwenhuizen <janneke@gnu.org>',
             'Han-Wen Nienhuys <hanwen@xs4all.nl>')

program_version = '@TOPLEVEL_VERSION@'

authors = ('Jan Nieuwenhuizen <janneke@gnu.org>',
           'Han-Wen Nienhuys <hanwen@xs4all.nl>')


def identify():
    ly.progress('%s (GNU LilyPond) %s\n' % (ly.program_name, program_version))


def warranty():
    identify()
    sys.stdout.write('''
%s

%s

%s
%s
''' % (_('Copyright (c) %s by') % '2001--2023',
        ' '.join(authors),
        _('Distributed under terms of the GNU General Public License.'),
        _('It comes with NO WARRANTY.')))


def get_option_parser():
    p = ly.get_option_parser(usage=_("%s [OPTION]... FILE") % 'convert-ly',
                             description=help_summary,
                             add_help_option=False)

    p.version = "@TOPLEVEL_VERSION@"
    p.add_option("--version",
                 action="version",
                 help=_("show version number and exit"))

    p.add_option("-h", "--help",
                 action="help",
                 help=_("show this help and exit"))

    p.add_option('-f', '--from',
                 action="store",
                 metavar=_("VERSION"),
                 dest="from_version",
                 help=_(
                     "start from VERSION [default: \\version found in file]"),
                 default='')

    p.add_option('-e', '--edit', help=_("edit in place"),
                 action='store_true')

    p.add_option("-l", "--loglevel",
                 help=_("Print log messages according to LOGLEVEL "
                        "(NONE, ERROR, WARNING, PROGRESS (default), DEBUG)"),
                 metavar=_("LOGLEVEL"),
                 action='callback',
                 callback=ly.handle_loglevel_option,
                 type='string')

    p.add_option('-n', '--no-version',
                 help=_("do not add \\version command if missing"),
                 action='store_true',
                 dest='skip_version_add',
                 default=False)

    p.add_option('-c', '--current-version',
                 help=_("force updating \\version number to %s") % program_version,
                 action='store_true',
                 dest='force_current_version',
                 default=False)

    p.add_option('-d', '--diff-version-update',
                 help=_("only update \\version number if file is modified"),
                 action='store_true',
                 dest='diff_version_update',
                 default=False)

    p.add_option("-s", '--show-rules',
                 help=_("show rules [default: -f 0, -t %s]") % program_version,
                 dest='show_rules',
                 action='store_true', default=False)

    p.add_option('-t', '--to',
                 help=_("convert to VERSION [default: %s]") % program_version,
                 metavar=_('VERSION'),
                 action='store',
                 dest="to_version",
                 default='')

    p.add_option('-b', '--backup-numbered',
                 help=_("make a numbered backup [default: filename.ext~]"),
                 action='store_true',
                 dest="backup_numbered",
                 default='')

    p.add_option('-w', '--warranty', help=_("show warranty and copyright"),
                 action='store_true',
                 ),
    p.add_option_group('',
                       description=(
                           _("Report bugs via %s")
                           % 'bug-lilypond@gnu.org') + '\n')

    return p


def str_to_tuple(s):
    return tuple([int(n) for n in s.split('.')])

def tup_to_str(t):
    return '.'.join(['%s' % x for x in t])

def latest_version():
    return convertrules.conversions[-1][0]


def show_rules(file, from_version, to_version):
    for x in convertrules.conversions:
        if (not from_version or x[0] > from_version) \
           and (not to_version or x[0] <= to_version):
            file.write('%s: %s\n' % (tup_to_str(x[0]), x[2]))

def do_conversion(s, from_version, to_version):
    """Apply conversions from FROM_VERSION to TO_VERSION.  Return
tuple (LAST,LASTCHANGED,STR,ERRORS), with the last applied conversion,
the last conversion resulting in a change, the resulting
string and the number of errors."""
    conv_list = [conv for conv in convertrules.conversions if from_version < conv[0] <= to_version]

    ly.progress(_("Applying conversion: "), newline=False)

    last_conversion = None
    last_change = None
    errors = 0
    try:
        for x in conv_list:
            if x != conv_list[-1]:
                ly.progress(tup_to_str(x[0]), newline=False)
                ly.progress(', ', newline=False)
            else:
                ly.progress(tup_to_str(x[0]))
            newstr = x[1](s)
            last_conversion = x[0]
            if newstr != s:
                last_change = last_conversion
            s = newstr

    except convertrules.FatalConversionError:
        ly.error(_("Error while converting")
                 + '\n'
                 + _("Stopping at last successful rule"))
        errors += 1

    return (last_conversion, last_change, s, errors)


def guess_lilypond_version(input):
    m = lilypond_version_strict_re.search(input)
    # Accept a missing third component if the second component
    # is even.  That works because we don't have conversion rules
    # within stable releases, as the syntax doesn't change.
    if m and (m.group(3) is not None or int(m.group(2))%2 == 0):
        return m.group(1)
    m = lilypond_version_re.search(input)
    if m:
        raise InvalidVersion(m.group(1))
    else:
        return ''


class FatalConversionError (Exception):
    pass


class UnknownVersion (Exception):
    pass


class InvalidVersion (Exception):
    def __init__(self, version):
        self.version = version


def back_up(file, numbered):
    if numbered:
        n = 0
        while True:
            n = n + 1
            back_up = file + '.~' + str(n) + '~'
            if not os.path.exists(back_up):
                break
    else:
        back_up = file + '~'
    shutil.copy2(file, back_up)
    return back_up


def do_one_file(infile_name):
    ly.progress(_("Processing `%s\'... ") % infile_name, True)

    if infile_name:
        infile = open(infile_name, 'rb')
        original = infile.read()
        infile.close()

        # Cope early with encoding change in 2.5.13: Try UTF-8 and attempt
        # conversion from latin1 if that fails.
        try:
            input = original.decode('utf-8')
        except UnicodeError:
            ly.progress(_("Attempting conversion from `latin1'..."))
            input = original.decode('latin1')

        # Convert platform-dependent newline character sequences
        # to `\n`. This is default behaviour when opening files in
        # text mode, which does not work for us, though, since we do not
        # know the encoding in advance.
        input = io.StringIO(input, newline=None).read()
    else:
        input = sys.stdin.read()

    to_version = None
    org_version = None
    guess = guess_lilypond_version(input)
    org_version = guess and str_to_tuple(guess)
    from_version = global_options.from_version or org_version
    if not from_version:
        raise UnknownVersion()

    if global_options.to_version:
        to_version = global_options.to_version
    else:
        to_version = latest_version()

    (last, last_change, result, errors) = \
        do_conversion(input, from_version, to_version)

    if global_options.force_current_version and \
            (last is None or last == to_version):
        last = str_to_tuple(program_version)
    if last:
        if global_options.diff_version_update:
            # Note that last_change can be set even if the result is
            # the same if two conversion rules cancelled out
            if result == input:
                # make no (actual) change to the version number
                last = org_version or from_version
            else:
                last = last_change
                # If the last update was to an unstable version
                # number, and the final update target is no longer in
                # the same unstable series, we update to the stable
                # series following the unstable version.
                if last[1] % 2:  # unstable
                    next_stable = (last[0], last[1]+1, 0)
                    if next_stable <= to_version:
                        last = next_stable

        newversion = r'\version "%s"' % tup_to_str(last)
        if lilypond_version_re.search(result):
            result = re.sub(lilypond_version_re_str,
                            '\\' + newversion, result)
        elif not global_options.skip_version_add:
            result = newversion + '\n' + result

    ly.progress('\n')

    if global_options.edit:
        backup = back_up(infile_name, global_options.backup_numbered)
        outfile = open(infile_name, 'w', encoding='utf-8')
    else:
        outfile = sys.stdout

    outfile.write(result)

    sys.stderr.flush()

    return errors


def do_options():
    opt_parser = get_option_parser()
    (options, args) = opt_parser.parse_args()

    if options.warranty:
        warranty()
        sys.exit(0)

    if options.from_version:
        options.from_version = str_to_tuple(options.from_version)
    if options.to_version:
        options.to_version = str_to_tuple(options.to_version)

    options.outfile_name = ''
    global global_options
    global_options = options

    if not args and not options.show_rules:
        opt_parser.print_help()
        sys.exit(2)

    return args


def main():
    files = do_options()

    # should parse files[] to read \version?
    if global_options.show_rules:
        show_rules(sys.stdout, global_options.from_version,
                   global_options.to_version)
        sys.exit(0)

    identify()

    errors = 0
    for f in files:
        if f == '-':
            f = ''
        elif not os.path.isfile(f):
            ly.error(_("%s: Unable to open file") % f)
            errors += 1
            continue
        try:
            errors += do_one_file(f)
        except UnknownVersion:
            ly.error(_("%s: Unable to determine version.  Skipping") % f)
            errors += 1
        except InvalidVersion as v:
            ly.error(_("%s: Invalid version string `%s' \n"
                       "Valid version strings consist of three numbers, "
                       "separated by dots, e.g. `2.8.12'") % (f, v.version))
            errors += 1

    if errors:
        ly.warning(gettext.ngettext("There was %d error.",
                                    "There were %d errors.", errors) % errors)
        sys.exit(1)


main()
