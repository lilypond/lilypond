#!@TARGET_PYTHON@
# -*- coding: utf-8 -*-

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 1998--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
#                           Jan Nieuwenhuizen <janneke@gnu.org>
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

r'''
Example usage:

test:
  lilypond-book --filter="tr '[a-z]' '[A-Z]'" BOOK

convert-ly on book:
  lilypond-book --filter="convert-ly --no-version --from=1.6.11 -" BOOK

classic lilypond-book:
  lilypond-book --process="lilypond" BOOK.tely

TODO:

  *  ly-options: intertext?
  *  --line-width?
  *  eps in latex / eps by lilypond -b ps?
  *  check latex parameters, twocolumn, multicolumn?
  *  use --png --ps --pdf for making images?

  *  Converting from lilypond-book source, substitute:
   @mbinclude foo.itely -> @include foo.itely
   \mbinput -> \input

'''


# TODO: Better solve the global_options copying to the snippets...

import gettext
import glob
import hashlib
from optparse import OptionGroup, SUPPRESS_HELP
import os
import re
import shlex
import stat
import subprocess
import sys
import tempfile
import typing

# See lock_path and unlock_path; this module is not available at all on Windows.
if os.name == 'posix':
    import fcntl

"""
@relocate-preamble@
"""

import book_base
import book_docbook
import book_html
import book_latex
import book_texinfo
import book_snippets

# Load translation and install _() into Python's builtins namespace.
gettext.install('lilypond', '@localedir@')

import lilylib as ly

backend = 'ps'

help_summary = (
    _("Process LilyPond snippets in hybrid HTML, LaTeX, texinfo or DocBook document.")
    + '\n\n'
    + _("Examples:")
    + '''
 $ lilypond-book --filter="tr '[a-z]' '[A-Z]'" %(BOOK)s
 $ lilypond-book -F "convert-ly --no-version --from=2.0.0 -" %(BOOK)s
 $ lilypond-book --process='lilypond -I include' %(BOOK)s
''' % {'BOOK': _("BOOK")})

authors = ('Jan Nieuwenhuizen <janneke@gnu.org>',
           'Han-Wen Nienhuys <hanwen@xs4all.nl>')

################################################################


def exit(i):
    if ly.is_verbose():
        raise Exception(_('Exiting (%d)...') % i)
    else:
        sys.exit(i)


progress = ly.progress
warning = ly.warning
error = ly.error

program_version = '@TOPLEVEL_VERSION@'
if program_version.startswith("@"):
    # '@' in lilypond-book output confuses texinfo
    program_version = "dev"


def identify():
    progress('%s (GNU LilyPond) %s' % (ly.program_name, program_version))


def warranty():
    identify()
    sys.stdout.write('''
%s

  %s

%s
%s
''' % (_('Copyright (c) %s by') % '2001--2023',
        '\n  '.join(authors),
        _("Distributed under terms of the GNU General Public License."),
        _("It comes with NO WARRANTY.")))


def get_option_parser():
    p = ly.get_option_parser(usage=_("%s [OPTION]... FILE") % 'lilypond-book',
                             description=help_summary,
                             conflict_handler="resolve",
                             add_help_option=False)

    p.add_option('-F', '--filter',
                 help=_("pipe snippets through FILTER "
                        "[default: `convert-ly -n -']"),
                 metavar=_("FILTER"),
                 action="store",
                 dest="filter_cmd",
                 default=None)

    p.add_option('-f', '--format',
                 help=_("use output format FORMAT (texi [default], "
                        "texi-html, latex, html, docbook)"),
                 metavar=_("FORMAT"),
                 action='store')

    p.add_option("-h", "--help",
                 action="help",
                 help=_("show this help and exit"))

    # Turn on syntax highlighting using vendored Pygments
    # when building the main documentation.  Purposefully
    # undocumented, it is not for end users.
    p.add_option("--highlight",
                 action="store_true",
                 help=SUPPRESS_HELP)

    p.add_option("-I", '--include',
                 help=_("add DIR to include path"),
                 metavar=_("DIR"),
                 action='append',
                 dest='include_path',
                 default=[])

    p.add_option('--info-images-dir',
                 help=_("format Texinfo output so that Info will "
                        "look for images of music in DIR"),
                 metavar=_("DIR"),
                 action='store',
                 dest='info_images_dir',
                 default='')

    p.add_option('--left-padding',
                 help=_("pad left side of music to align music in spite "
                        "of uneven bar numbers (in mm) [default: %default]"),
                 metavar=_("PAD"),
                 dest="padding_mm",
                 type="float",
                 default=3.0)

    p.add_option('--lily-loglevel',
                 help=_("print lilypond log messages according to LOGLEVEL "
                        "[default: %default]"),
                 metavar=_("LOGLEVEL"),
                 action='store',
                 dest='lily_loglevel',
                 default=os.environ.get("LILYPOND_LOGLEVEL", None))

    p.add_option('--lily-output-dir',
                 help=_("write lily-XXX files to DIR, "
                        "link into --output dir"),
                 metavar=_("DIR"),
                 action='store',
                 dest='lily_output_dir',
                 default=None)

    p.add_option("-l", "--loglevel",
                 help=_("print log messages according to LOGLEVEL "
                        "(NONE, ERROR, WARNING, PROGRESS [default], DEBUG)"),
                 metavar=_("LOGLEVEL"),
                 action='callback',
                 callback=ly.handle_loglevel_option,
                 type='string')

    p.add_option("-o", '--output',
                 help=_("write output to DIR"),
                 metavar=_("DIR"),
                 action='store',
                 dest='output_dir',
                 default='')

    p.add_option('-P', '--process',
                 help=_("process ly_files using COMMAND FILE..."),
                 metavar=_("COMMAND"),
                 action='store',
                 dest='process_cmd',
                 default='')

    p.add_option('--redirect-lilypond-output',
                 help=_("redirect the lilypond output"),
                 action='store_true',
                 dest='redirect_output',
                 default=False)

    p.add_option('-s', '--safe',
                 help=_("removed; using this option results in an error"),
                 action="store_true",
                 dest="safe_mode",
                 default=False)

    p.add_option('--skip-lily-check',
                 help=_("do not fail if no lilypond output is found"),
                 metavar=_("DIR"),
                 action='store_true',
                 dest='skip_lilypond_run',
                 default=False)

    p.add_option('--skip-png-check',
                 help=_("do not fail if no PNG images "
                        "are found for EPS files"),
                 metavar=_("DIR"),
                 action='store_true',
                 dest='skip_png_check',
                 default=False)

    p.add_option('--use-source-file-names',
                 help=_("write snippet output files with the same "
                        "base name as their source file"),
                 action='store_true',
                 dest='use_source_file_names',
                 default=False)

    p.add_option('-V', '--verbose',
                 help=_("be verbose"),
                 action="callback",
                 callback=ly.handle_loglevel_option,
                 callback_args=("DEBUG",))

    p.version = "@TOPLEVEL_VERSION@"
    p.add_option("--version",
                 help=_("show version number and exit"),
                 action="version")

    p.add_option('-w', '--warranty',
                 help=_("show warranty and copyright"),
                 action='store_true')

    group = OptionGroup(p, "Options only for the latex and texinfo backends")
    group.add_option('--latex-program',
                     help=_("run executable PROG instead of latex or, "
                            "in case --pdf option is set, "
                            "instead of pdflatex"),
                     metavar=_("PROG"),
                     action='store',
                     dest='latex_program',
                     default='latex')
    group.add_option('--texinfo-program',
                     help=_("run executable PROG instead of texi2pdf"),
                     metavar=_("PROG"),
                     action='store',
                     dest='texinfo_program',
                     default='texi2pdf')
    group.add_option('--pdf',
                     help=_("create PDF files for use with pdftex"),
                     action="store_true",
                     dest="create_pdf",
                     default=False)
    p.add_option_group(group)

    p.add_option_group('',
                       description=(
                           _("Report bugs via %s")
                           % 'bug-lilypond@gnu.org') + '\n')

    for formatter in book_base.all_formats:
        formatter.add_options(p)

    return p


lilypond_binary = os.path.join('@bindir@', 'lilypond')

# If we are called with full path, try to use lilypond binary
# installed in the same path; this is needed in GUB binaries, where
# @bindir is always different from the installed binary path.
if 'bindir' in globals() and bindir:
    lilypond_binary = os.path.join(bindir, 'lilypond')

# Only use installed binary when we are installed too.
if '@bindir@' == ('@' + 'bindir@') or not os.path.exists(lilypond_binary):
    lilypond_binary = 'lilypond'

# Need to shell-quote, issue 3468
# FIXME: we should really pass argument lists
# everywhere instead of playing with shell syntax.
lilypond_binary = shlex.quote(lilypond_binary)

global_options = None


def command_name(cmd):
    # Strip all stuf after command,
    # deal with "((latex ) >& 1 ) .." too
    cmd = re.match(r'([\(\)]*)([^\\ ]*)', cmd).group(2)
    return os.path.basename(cmd)


def system_in_directory(cmd_str, directory, log_file):
    """Execute a command in a different directory."""

    if ly.is_verbose():
        ly.progress(_("Invoking `%s\'") % cmd_str)
    elif global_options.redirect_output:
        ly.progress(_("Processing %s.ly") % log_file)
    else:
        name = command_name(cmd_str)
        ly.progress(_("Running %s...") % name)

    output_location = None
    if global_options.redirect_output:
        output_location = open(log_file + '.log', 'w', encoding='utf-8')

    try:
        subprocess.run(cmd_str, stdout=output_location,
                       stderr=output_location, cwd=directory,
                       shell=True, check=True)
    except subprocess.CalledProcessError as e:
        sys.stderr.write("%s\n" % e)
        sys.exit(1)


def process_snippets(cmd, outdated_dict,
                     formatter, lily_output_dir):
    """Run cmd on all of the .ly files from snippets."""
    basenames = sorted(outdated_dict.keys())

    # No need for a secure hash function, just need a digest.
    checksum = hashlib.md5()
    for name in basenames:
        checksum.update(name.encode('ascii'))
    checksum = checksum.hexdigest()

    lily_output_dir = global_options.lily_output_dir

    # Write list of snippet names.
    snippet_names_file = 'snippet-names-%s.ly' % checksum
    snippet_names_path = os.path.join(lily_output_dir, snippet_names_file)
    with open(snippet_names_path, 'w', encoding='utf-8') as snippet_names:
        snippet_names.write('\n'.join([name + '.ly' for name in basenames]))

    # Run command.
    cmd = formatter.adjust_snippet_command(cmd)
    # Remove .ly ending.
    logfile = os.path.splitext(snippet_names_path)[0]
    snippet_names_arg = mkarg(snippet_names_path.replace(os.path.sep, '/'))
    system_in_directory(' '.join([cmd, snippet_names_arg]),
                        lily_output_dir,
                        logfile)
    os.unlink(snippet_names_path)


def lock_path(name):
    if os.name != 'posix':
        return None

    fp = open(name, 'w', encoding='utf-8')
    fcntl.lockf(fp, fcntl.LOCK_EX)
    return fp


def unlock_path(lock):
    if os.name != 'posix':
        return None
    fcntl.lockf(lock, fcntl.LOCK_UN)
    lock.close()


def do_process_cmd(chunks, options):
    """Wrap do_process_cmd_locked in a filesystem lock"""
    snippets = [c for c in chunks if isinstance(
        c, book_snippets.LilypondSnippet)]

    # calculate checksums eagerly
    for s in snippets:
        s.get_checksum()

    os.makedirs(options.lily_output_dir, exist_ok=True)
    lock_file = os.path.join(options.lily_output_dir, "lock")
    lock = None
    try:
        lock = lock_path(lock_file)
        do_process_cmd_locked(snippets, options)
    finally:
        if lock:
            unlock_path(lock)


def do_process_cmd_locked(snippets, options):
    """Look at all snippets, write the outdated ones, and compile them."""
    outdated = [c for c in snippets if c.is_outdated(options.lily_output_dir)]

    if outdated:
        # First unique the list based on the basename, by using them as keys
        # in a dict.
        outdated_dict = dict()
        for snippet in outdated:
            outdated_dict[snippet.basename()] = snippet

        # Next call write_ly() for each snippet once.
        progress(_("Writing snippets..."))
        for snippet in outdated_dict.values():
            snippet.write_ly()

        progress(_("Processing..."))
        process_snippets(options.process_cmd, outdated_dict,
                         options.formatter, options.lily_output_dir)

    else:
        progress(_("All snippets are up to date..."))

    progress(_("Linking files..."))
    if options.lily_output_dir != options.output_dir:
        for snippet in snippets:
            snippet.link_all_output_files(options.lily_output_dir,
                                          options.output_dir)


###
# Format guessing data

def guess_format(input_filename):
    format = None
    e = os.path.splitext(input_filename)[1]
    for formatter in book_base.all_formats:
        if formatter.can_handle_extension(e):
            return formatter
    error(_("cannot determine format for: %s" % input_filename))
    exit(1)

def write_if_updated(file_name, lines):
    try:
        with open(file_name, encoding='utf-8') as file:
            old_str = file.read()
    except FileNotFoundError:
        pass
    else:
        new_str = ''.join(lines)
        if old_str == new_str:
            progress(_("%s is up to date.") % file_name)

            # this prevents make from always rerunning lilypond-book:
            # output file must be touched in order to be up to date
            os.utime(file_name, None)
            return

    output_dir = os.path.dirname(file_name)
    os.makedirs(output_dir, exist_ok=True)

    progress(_("Writing `%s'...") % file_name)
    open(file_name, 'w', encoding='utf-8').writelines(lines)


def note_input_file(name, inputs=[]):
    # hack: inputs is mutable!
    inputs.append(name)
    return inputs


def samefile(f1, f2):
    try:
        return os.path.samefile(f1, f2)
    except AttributeError:                # Windoze
        f1 = re.sub("//*", "/", f1)
        f2 = re.sub("//*", "/", f2)
        return f1 == f2


def do_file(input_filename, included=False):
    # Ugh.
    input_absname = input_filename
    if not input_filename or input_filename == '-':
        in_handle = sys.stdin
    else:
        if os.path.exists(input_filename):
            input_fullname = input_filename
        else:
            input_fullname = global_options.formatter.input_fullname(
                input_filename)
        # Normalize path to absolute path, since we will change cwd to the output dir!
        # Otherwise, "lilypond-book -o out test.tex" will complain that it is
        # overwriting the input file (which it is actually not), since the
        # input filename is relative to the CWD...
        input_absname = os.path.abspath(input_fullname)

        note_input_file(input_fullname)
        in_handle = open(input_fullname, 'r', encoding='utf-8')

    if input_filename == '-':
        global_options.input_dir = os.getcwd()
        input_base = 'stdin'
    elif included:
        input_base = os.path.splitext(input_filename)[0]
    else:
        global_options.input_dir = os.path.split(input_absname)[0]
        input_base = os.path.basename(
            os.path.splitext(input_filename)[0])

    output_filename = os.path.join(global_options.output_dir,
                                   input_base + global_options.formatter.default_extension)
    if (os.path.exists(input_filename)
        and os.path.exists(output_filename)
            and samefile(output_filename, input_absname)):
        error(
            _("Output would overwrite input file; use --output."))
        exit(2)

    try:
        progress(_("Reading `%s'") % input_absname)
        source = in_handle.read()

        if not included:
            global_options.formatter.init_default_snippet_options(source)

        progress(_("Dissecting..."))
        chunks = book_base.find_toplevel_snippets(
            source, global_options.formatter, global_options)
        for c in chunks:
            c.set_output_fullpath(output_filename)

        # Let the formatter modify the chunks before further processing
        chunks = global_options.formatter.process_chunks(chunks)

        def process_include(snippet):
            name = snippet.substring('filename')
            progress(_("Processing include `%s'") % name)
            return do_file(name, included=True)

        include_chunks = []
        for x in chunks:
            if isinstance(x, book_snippets.IncludeSnippet):
               include_chunks += process_include(x)

        return chunks + include_chunks

    except book_snippets.CompileError:
        progress(_("Removing `%s'") % output_filename)
        raise book_snippets.CompileError


def do_options():
    global global_options

    opt_parser = get_option_parser()
    (global_options, args) = opt_parser.parse_args()

    if global_options.safe_mode:
        error("""Due to security vulnerabilities deemed unfixable
by the developers, LilyPond's safe mode was removed in
version 2.23.12 in order not to provide a false sense of
security.  If you need to compile an untrusted .ly file, please
use an external tool to run LilyPond in a sandbox.""")
        raise SystemExit

    global_options.information = {
        'program_version': program_version, 'program_name': ly.program_name}

    if global_options.lily_output_dir:
        global_options.lily_output_dir = os.path.expanduser(
            global_options.lily_output_dir)
    if global_options.output_dir:
        global_options.output_dir = os.path.expanduser(
            global_options.output_dir)

    # Compute absolute paths of include directories.
    for i, path in enumerate(global_options.include_path):
        global_options.include_path[i] = os.path.abspath(path)

    # Append the current directory.
    global_options.include_path.append(os.getcwd())

    if global_options.warranty:
        warranty()
        exit(0)
    if not args or len(args) > 1:
        opt_parser.print_help()
        exit(2)

    return args


def mkarg(x):
    r"""
    A modified version of the commands.mkarg(x)

    Uses double quotes (since Windows can't handle the single quotes)
    and escapes the characters \, $, ", and ` for unix shells.
    """
    if os.name == 'nt':
        return ' "%s"' % x
    s = ' "'
    for c in x:
        if c in '\\$"`':
            s = s + '\\'
        s = s + c
    s = s + '"'
    return s


def write_output_documents(chunks: typing.List[book_snippets.Chunk], is_filter: bool):
    text_by_path = {}
    for ch in chunks:
        path = ch.output_fullpath()
        if path not in text_by_path:
            text_by_path[path] = []

        if is_filter:
            s = ch.filter_text()
        else:
            s = ch.replacement_text()

        text_by_path[path].append(s)

    for path in text_by_path:
        write_if_updated(path, text_by_path[path])


def main():
    if "LILYPOND_BOOK_LOGLEVEL" in os.environ:
        ly.set_loglevel(os.environ["LILYPOND_BOOK_LOGLEVEL"])
    files = do_options()

    basename = os.path.splitext(files[0])[0]
    basename = os.path.split(basename)[1]

    if global_options.format:
        # Retrieve the formatter for the given format
        for formatter in book_base.all_formats:
            if formatter.can_handle_format(global_options.format):
                global_options.formatter = formatter
    else:
        global_options.formatter = guess_format(files[0])
        global_options.format = global_options.formatter.format

    # make the global options available to the formatters:
    global_options.formatter.global_options = global_options
    formats = global_options.formatter.image_formats

    if global_options.process_cmd == '':
        global_options.process_cmd = (
            lilypond_binary + ' --formats=%s ' % formats)

    global_options.process_cmd += (
        ' '.join([' -I %s' % mkarg(p) for p in global_options.include_path])
        + ' -daux-files ')

    global_options.formatter.process_options(global_options)

    if global_options.lily_loglevel:
        ly.debug_output(_("Setting LilyPond's loglevel to %s") %
                        global_options.lily_loglevel, True)
        global_options.process_cmd += " --loglevel=%s" % global_options.lily_loglevel
    elif ly.is_verbose():
        if os.environ.get("LILYPOND_LOGLEVEL", None):
            ly.debug_output(_("Setting LilyPond's loglevel to %s (from environment variable LILYPOND_LOGLEVEL)") %
                            os.environ.get("LILYPOND_LOGLEVEL", None), True)
            global_options.process_cmd += " --loglevel=%s" % os.environ.get(
                "LILYPOND_LOGLEVEL", None)
        else:
            ly.debug_output(
                _("Setting LilyPond's output to --verbose, implied by lilypond-book's setting"), True)
            global_options.process_cmd += " --verbose"

    global_options.process_cmd += " -dread-file-list -dno-strip-output-dir"

    # Store the original argument to construct the dependency file below.
    relative_output_dir = global_options.output_dir

    if global_options.output_dir:
        global_options.output_dir = os.path.abspath(global_options.output_dir)
        # Create the directory, but do not complain if it already exists.
        os.makedirs(global_options.output_dir, exist_ok=True)
    else:
        global_options.output_dir = os.getcwd()

    if global_options.lily_output_dir:
        global_options.lily_output_dir = os.path.abspath(
            global_options.lily_output_dir)
    else:
        global_options.lily_output_dir = global_options.output_dir

    identify()
    try:
        chunks = do_file(files[0])
        if global_options.filter_cmd:
            write_output_documents(chunks, is_filter=True)
        elif global_options.process_cmd:
            do_process_cmd(chunks, global_options)
            progress(_("Compiling `%s'...") % files[0])
            write_output_documents(chunks, is_filter=False)
    except book_snippets.CompileError:
        exit(1)

    inputs = note_input_file('')
    inputs.pop()

    base_file_name = os.path.splitext(os.path.basename(files[0]))[0]
    dep_file = os.path.join(global_options.output_dir, base_file_name + '.dep')
    final_output_file = os.path.join(relative_output_dir,
                                     base_file_name + global_options.formatter.default_extension)
    open(dep_file, 'w', encoding='utf-8').write('%s: %s\n'
                              % (final_output_file, ' '.join(inputs)))


if __name__ == '__main__':
    main()
