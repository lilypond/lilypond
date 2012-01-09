#!@TARGET_PYTHON@
# -*- coding: utf-8 -*-

# This file is part of LilyPond, the GNU music typesetter.
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

'''
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

import glob
import os
import re
import stat
import sys
import tempfile
import imp
from optparse import OptionGroup


"""
@relocate-preamble@
"""

import lilylib as ly
import fontextract
import langdefs
global _;_=ly._

import book_base as BookBase
import book_snippets as BookSnippet
import book_html
import book_docbook
import book_texinfo
import book_latex

ly.require_python_version ()

original_dir = os.getcwd ()
backend = 'ps'

help_summary = (
_ ("Process LilyPond snippets in hybrid HTML, LaTeX, texinfo or DocBook document.")
+ '\n\n'
+ _ ("Examples:")
+ '''
 $ lilypond-book --filter="tr '[a-z]' '[A-Z]'" %(BOOK)s
 $ lilypond-book -F "convert-ly --no-version --from=2.0.0 -" %(BOOK)s
 $ lilypond-book --process='lilypond -I include' %(BOOK)s
''' % {'BOOK': _ ("BOOK")})

authors = ('Jan Nieuwenhuizen <janneke@gnu.org>',
           'Han-Wen Nienhuys <hanwen@xs4all.nl>')

################################################################
def exit (i):
    if ly.is_verbose ():
        raise Exception (_ ('Exiting (%d)...') % i)
    else:
        sys.exit (i)

def identify ():
    ly.encoded_write (sys.stdout, '%s (GNU LilyPond) %s\n' % (ly.program_name, ly.program_version))

progress = ly.progress
warning = ly.warning
error = ly.error


def warranty ():
    identify ()
    ly.encoded_write (sys.stdout, '''
%s

  %s

%s
%s
''' % ( _ ('Copyright (c) %s by') % '2001--2012',
        '\n  '.join (authors),
        _ ("Distributed under terms of the GNU General Public License."),
        _ ("It comes with NO WARRANTY.")))


def get_option_parser ():
    p = ly.get_option_parser (usage=_ ("%s [OPTION]... FILE") % 'lilypond-book',
                              description=help_summary,
                              conflict_handler="resolve",
                              add_help_option=False)

    p.add_option ('-F', '--filter', metavar=_ ("FILTER"),
                  action="store",
                  dest="filter_cmd",
                  help=_ ("pipe snippets through FILTER [default: `convert-ly -n -']"),
                  default=None)

    p.add_option ('-f', '--format',
                  help=_ ("use output format FORMAT (texi [default], texi-html, latex, html, docbook)"),
                  metavar=_ ("FORMAT"),
                  action='store')

    p.add_option("-h", "--help",
                 action="help",
                 help=_ ("show this help and exit"))

    p.add_option ("-I", '--include', help=_ ("add DIR to include path"),
                  metavar=_ ("DIR"),
                  action='append', dest='include_path',
                  default=[os.path.abspath (os.getcwd ())])

    p.add_option ('--info-images-dir',
                  help=_ ("format Texinfo output so that Info will "
                          "look for images of music in DIR"),
                  metavar=_ ("DIR"),
                  action='store', dest='info_images_dir',
                  default='')

    p.add_option ('--left-padding',
                  metavar=_ ("PAD"),
                  dest="padding_mm",
                  help=_ ("pad left side of music to align music inspite of uneven bar numbers (in mm)"),
                  type="float",
                  default=3.0)

    p.add_option ('--lily-loglevel',
                  help=_ ("Print lilypond log messages according to LOGLEVEL"),
                  metavar=_ ("LOGLEVEL"),
                  action='store', dest='lily_loglevel',
                  default=os.environ.get ("LILYPOND_LOGLEVEL", None))

    p.add_option ('--lily-output-dir',
                  help=_ ("write lily-XXX files to DIR, link into --output dir"),
                  metavar=_ ("DIR"),
                  action='store', dest='lily_output_dir',
                  default=None)

    p.add_option ('--load-custom-package', help=_ ("Load the additional python PACKAGE (containing e.g. a custom output format)"),
                  metavar=_ ("PACKAGE"),
                  action='append', dest='custom_packages',
                  default=[])

    p.add_option ("-l", "--loglevel",
                  help=_ ("Print log messages according to LOGLEVEL "
                          "(NONE, ERROR, WARNING, PROGRESS (default), DEBUG)"),
                  metavar=_ ("LOGLEVEL"),
                  action='callback',
                  callback=ly.handle_loglevel_option,
                  type='string')

    p.add_option ("-o", '--output', help=_ ("write output to DIR"),
                  metavar=_ ("DIR"),
                  action='store', dest='output_dir',
                  default='')

    p.add_option ('-P', '--process', metavar=_ ("COMMAND"),
                  help = _ ("process ly_files using COMMAND FILE..."),
                  action='store',
                  dest='process_cmd', default='')

    p.add_option ('--redirect-lilypond-output',
                  help = _ ("Redirect the lilypond output"),
                  action='store_true',
                  dest='redirect_output', default=False)

    p.add_option ('-s', '--safe', help=_ ("Compile snippets in safe mode"),
                  action="store_true",
                  default=False,
                  dest="safe_mode")

    p.add_option ('--skip-lily-check',
                  help=_ ("do not fail if no lilypond output is found"),
                  metavar=_ ("DIR"),
                  action='store_true', dest='skip_lilypond_run',
                  default=False)

    p.add_option ('--skip-png-check',
                  help=_ ("do not fail if no PNG images are found for EPS files"),
                  metavar=_ ("DIR"),
                  action='store_true', dest='skip_png_check',
                  default=False)

    p.add_option ('--use-source-file-names',
                  help=_ ("write snippet output files with the same base name as their source file"),
                  action='store_true', dest='use_source_file_names',
                  default=False)

    p.add_option ('-V', '--verbose', help=_ ("be verbose"),
                  action="callback",
                  callback=ly.handle_loglevel_option,
                  callback_args=("DEBUG",))

    p.version = "@TOPLEVEL_VERSION@"
    p.add_option("--version",
                 action="version",
                 help=_ ("show version number and exit"))

    p.add_option ('-w', '--warranty',
                  help=_ ("show warranty and copyright"),
                  action='store_true')

    group = OptionGroup (p, "Options only for the latex and texinfo backends")
    group.add_option ('--latex-program',
              help=_ ("run executable PROG instead of latex, or in\n\
case --pdf option is set instead of pdflatex"),
              metavar=_ ("PROG"),
              action='store', dest='latex_program',
              default='latex')
    group.add_option ('--texinfo-program',
              help=_ ("run executable PROG instead of texi2pdf"),
              metavar=_ ("PROG"),
              action='store', dest='texinfo_program',
              default='texi2pdf')
    group.add_option ('--pdf',
              action="store_true",
              dest="create_pdf",
              help=_ ("create PDF files for use with PDFTeX"),
              default=False)
    p.add_option_group (group)

    p.add_option_group ('',
                        description=(
        _ ("Report bugs via %s")
        % ' http://post.gmane.org/post.php'
        '?group=gmane.comp.gnu.lilypond.bugs') + '\n')


    for formatter in BookBase.all_formats:
      formatter.add_options (p)

    return p

lilypond_binary = os.path.join ('@bindir@', 'lilypond')

# If we are called with full path, try to use lilypond binary
# installed in the same path; this is needed in GUB binaries, where
# @bindir is always different from the installed binary path.
if 'bindir' in globals () and bindir:
    lilypond_binary = os.path.join (bindir, 'lilypond')

# Only use installed binary when we are installed too.
if '@bindir@' == ('@' + 'bindir@') or not os.path.exists (lilypond_binary):
    lilypond_binary = 'lilypond'

global_options = None




def find_linestarts (s):
    nls = [0]
    start = 0
    end = len (s)
    while 1:
        i = s.find ('\n', start)
        if i < 0:
            break

        i = i + 1
        nls.append (i)
        start = i

    nls.append (len (s))
    return nls

def find_toplevel_snippets (input_string, formatter):
    res = {}
    types = formatter.supported_snippet_types ()
    for t in types:
        res[t] = re.compile (formatter.snippet_regexp (t))

    snippets = []
    index = 0
    found = dict ([(t, None) for t in types])

    line_starts = find_linestarts (input_string)
    line_start_idx = 0
    # We want to search for multiple regexes, without searching
    # the string multiple times for one regex.
    # Hence, we use earlier results to limit the string portion
    # where we search.
    # Since every part of the string is traversed at most once for
    # every type of snippet, this is linear.
    while 1:
        first = None
        endex = 1 << 30
        for type in types:
            if not found[type] or found[type][0] < index:
                found[type] = None

                m = res[type].search (input_string[index:endex])
                if not m:
                    continue

                klass = global_options.formatter.snippet_class (type)

                start = index + m.start ('match')
                line_number = line_start_idx
                while (line_starts[line_number] < start):
                    line_number += 1

                line_number += 1
                snip = klass (type, m, formatter, line_number, global_options)

                found[type] = (start, snip)

            if (found[type]
                and (not first
                     or found[type][0] < found[first][0])):
                first = type

                # FIXME.

                # Limiting the search space is a cute
                # idea, but this *requires* to search
                # for possible containing blocks
                # first, at least as long as we do not
                # search for the start of blocks, but
                # always/directly for the entire
                # @block ... @end block.

                endex = found[first][0]

        if not first:
            snippets.append (BookSnippet.Substring (input_string, index, len (input_string), line_start_idx))
            break

        while (start > line_starts[line_start_idx+1]):
            line_start_idx += 1

        (start, snip) = found[first]
        snippets.append (BookSnippet.Substring (input_string, index, start, line_start_idx + 1))
        snippets.append (snip)
        found[first] = None
        index = start + len (snip.match.group ('match'))

    return snippets

def system_in_directory (cmd, directory, logfile):
    """Execute a command in a different directory.

    Because of win32 compatibility, we can't simply use subprocess.
    """

    current = os.getcwd()
    os.chdir (directory)
    """NB - ignore_error is deliberately set to the same value
    as redirect_output - this is not a typo."""
    retval = ly.system(cmd,
              be_verbose=ly.is_verbose (),
              redirect_output=global_options.redirect_output,
              log_file=logfile,
              progress_p=1,
              ignore_error=global_options.redirect_output)
    if retval != 0:
        print ("Error trapped by lilypond-book")
        print ("\nPlease see " + logfile + ".log\n")
        sys.exit(1)

    os.chdir (current)


def process_snippets (cmd, snippets,
                      formatter, lily_output_dir):
    """Run cmd on all of the .ly files from snippets."""

    if not snippets:
        return

    cmd = formatter.adjust_snippet_command (cmd)

    checksum = snippet_list_checksum (snippets)
    contents = '\n'.join (['snippet-map-%d.ly' % checksum]
                          + list (set ([snip.basename() + '.ly' for snip in snippets])))
    name = os.path.join (lily_output_dir,
                         'snippet-names-%d.ly' % checksum)
    logfile = name.replace('.ly', '')
    file (name, 'wb').write (contents)

    system_in_directory (' '.join ([cmd, ly.mkarg (name)]),
                         lily_output_dir,
                         logfile)

def snippet_list_checksum (snippets):
    return hash (' '.join([l.basename() for l in snippets]))

def write_file_map (lys, name):
    snippet_map = file (os.path.join (
        global_options.lily_output_dir,
        'snippet-map-%d.ly' % snippet_list_checksum (lys)), 'w')

    snippet_map.write ("""
#(define version-seen #t)
#(define output-empty-score-list #f)
#(ly:add-file-name-alist '(%s
    ))\n
""" % '\n'.join(['("%s.ly" . "%s")\n' % (ly.basename ().replace('\\','/'), name)
                 for ly in lys]))

def split_output_files(directory):
    """Returns directory entries in DIRECTORY/XX/ , where XX are hex digits.

    Return value is a set of strings.
    """
    files = []
    for subdir in glob.glob (os.path.join (directory, '[a-f0-9][a-f0-9]')):
        base_subdir = os.path.split (subdir)[1]
        sub_files = [os.path.join (base_subdir, name)
                     for name in os.listdir (subdir)]
        files += sub_files
    return set (files)

def do_process_cmd (chunks, input_name, options):
    snippets = [c for c in chunks if isinstance (c, BookSnippet.LilypondSnippet)]

    output_files = split_output_files (options.lily_output_dir)
    outdated = [c for c in snippets if c.is_outdated (options.lily_output_dir, output_files)]

    write_file_map (outdated, input_name)
    progress (_ ("Writing snippets..."))
    for snippet in outdated:
        snippet.write_ly()

    if outdated:
        progress (_ ("Processing..."))
        process_snippets (options.process_cmd, outdated,
                          options.formatter, options.lily_output_dir)

    else:
        progress (_ ("All snippets are up to date..."))

    if options.lily_output_dir != options.output_dir:
        output_files = split_output_files (options.lily_output_dir)
        for snippet in snippets:
            snippet.link_all_output_files (options.lily_output_dir,
                                           output_files,
                                           options.output_dir)


###
# Format guessing data

def guess_format (input_filename):
    format = None
    e = os.path.splitext (input_filename)[1]
    for formatter in BookBase.all_formats:
      if formatter.can_handle_extension (e):
        return formatter
    error (_ ("cannot determine format for: %s" % input_filename))
    exit (1)

def write_if_updated (file_name, lines):
    try:
        f = file (file_name)
        oldstr = f.read ()
        new_str = ''.join (lines)
        if oldstr == new_str:
            progress (_ ("%s is up to date.") % file_name)

            # this prevents make from always rerunning lilypond-book:
            # output file must be touched in order to be up to date
            os.utime (file_name, None)
            return
    except:
        pass

    output_dir = os.path.dirname (file_name)
    if not os.path.exists (output_dir):
        os.makedirs (output_dir)

    progress (_ ("Writing `%s'...") % file_name)
    file (file_name, 'w').writelines (lines)


def note_input_file (name, inputs=[]):
    ## hack: inputs is mutable!
    inputs.append (name)
    return inputs

def samefile (f1, f2):
    try:
        return os.path.samefile (f1, f2)
    except AttributeError:                # Windoze
        f1 = re.sub ("//*", "/", f1)
        f2 = re.sub ("//*", "/", f2)
        return f1 == f2

def do_file (input_filename, included=False):
    # Ugh.
    input_absname = input_filename
    if not input_filename or input_filename == '-':
        in_handle = sys.stdin
        input_fullname = '<stdin>'
    else:
        if os.path.exists (input_filename):
            input_fullname = input_filename
        else:
            input_fullname = global_options.formatter.input_fullname (input_filename)
        # Normalize path to absolute path, since we will change cwd to the output dir!
        # Otherwise, "lilypond-book -o out test.tex" will complain that it is
        # overwriting the input file (which it is actually not), since the
        # input filename is relative to the CWD...
        input_absname = os.path.abspath (input_fullname)

        note_input_file (input_fullname)
        in_handle = file (input_fullname)

    if input_filename == '-':
        input_base = 'stdin'
    elif included:
        input_base = os.path.splitext (input_filename)[0]
    else:
        input_base = os.path.basename (
            os.path.splitext (input_filename)[0])

    # don't complain when global_options.output_dir is existing
    if not global_options.output_dir:
        global_options.output_dir = os.getcwd()
    else:
        global_options.output_dir = os.path.abspath(global_options.output_dir)

        if not os.path.isdir (global_options.output_dir):
            os.mkdir (global_options.output_dir, 0777)
        os.chdir (global_options.output_dir)

    output_filename = os.path.join(global_options.output_dir,
                                   input_base + global_options.formatter.default_extension)
    if (os.path.exists (input_filename)
        and os.path.exists (output_filename)
        and samefile (output_filename, input_absname)):
     error (
     _ ("Output would overwrite input file; use --output."))
     exit (2)

    try:
        progress (_ ("Reading %s...") % input_fullname)
        source = in_handle.read ()

        if not included:
            global_options.formatter.init_default_snippet_options (source)


        progress (_ ("Dissecting..."))
        chunks = find_toplevel_snippets (source, global_options.formatter)

        # Let the formatter modify the chunks before further processing
        chunks = global_options.formatter.process_chunks (chunks)

        if global_options.filter_cmd:
            write_if_updated (output_filename,
                     [c.filter_text () for c in chunks])
        elif global_options.process_cmd:
            do_process_cmd (chunks, input_fullname, global_options)
            progress (_ ("Compiling %s...") % output_filename)
            write_if_updated (output_filename,
                     [s.replacement_text ()
                     for s in chunks])

        def process_include (snippet):
            os.chdir (original_dir)
            name = snippet.substring ('filename')
            progress (_ ("Processing include: %s") % name)
            return do_file (name, included=True)

        include_chunks = map (process_include,
                              filter (lambda x: isinstance (x, BookSnippet.IncludeSnippet),
                                      chunks))

        return chunks + reduce (lambda x, y: x + y, include_chunks, [])

    except BookSnippet.CompileError:
        os.chdir (original_dir)
        progress (_ ("Removing `%s'") % output_filename)
        raise BookSnippet.CompileError

def do_options ():
    global global_options

    opt_parser = get_option_parser()
    (global_options, args) = opt_parser.parse_args ()

    global_options.information = {'program_version': ly.program_version, 'program_name': ly.program_name }

    global_options.include_path =  map (os.path.abspath, global_options.include_path)

    # Load the python packages (containing e.g. custom formatter classes)
    # passed on the command line
    nr = 0
    for i in global_options.custom_packages:
        nr += 1
        print imp.load_source ("book_custom_package%s" % nr, i)


    if global_options.warranty:
        warranty ()
        exit (0)
    if not args or len (args) > 1:
        opt_parser.print_help ()
        exit (2)

    return args

def main ():
    # FIXME: 85 lines of `main' macramee??
    if (os.environ.has_key ("LILYPOND_BOOK_LOGLEVEL")):
        ly.set_loglevel (os.environ["LILYPOND_BOOK_LOGLEVEL"])
    files = do_options ()

    basename = os.path.splitext (files[0])[0]
    basename = os.path.split (basename)[1]

    if global_options.format:
      # Retrieve the formatter for the given format
      for formatter in BookBase.all_formats:
        if formatter.can_handle_format (global_options.format):
          global_options.formatter = formatter
    else:
        global_options.formatter = guess_format (files[0])
        global_options.format = global_options.formatter.format

    # make the global options available to the formatters:
    global_options.formatter.global_options = global_options
    formats = global_options.formatter.image_formats

    if global_options.process_cmd == '':
        global_options.process_cmd = (lilypond_binary
                                      + ' --formats=%s -dbackend=eps ' % formats)

    if global_options.process_cmd:
        includes = global_options.include_path
        if global_options.lily_output_dir:
            # This must be first, so lilypond prefers to read .ly
            # files in the other lybookdb dir.
            includes = [os.path.abspath(global_options.lily_output_dir)] + includes
        global_options.process_cmd += ' '.join ([' -I %s' % ly.mkarg (p)
                                                 for p in includes])

    global_options.formatter.process_options (global_options)

    if global_options.lily_loglevel:
        ly.debug_output (_ ("Setting LilyPond's loglevel to %s") % global_options.lily_loglevel, True)
        global_options.process_cmd += " --loglevel=%s" % global_options.lily_loglevel
    elif ly.is_verbose ():
        if os.environ.get ("LILYPOND_LOGLEVEL", None):
            ly.debug_output (_ ("Setting LilyPond's loglevel to %s (from environment variable LILYPOND_LOGLEVEL)") % os.environ.get ("LILYPOND_LOGLEVEL", None), True)
            global_options.process_cmd += " --loglevel=%s" % os.environ.get ("LILYPOND_LOGLEVEL", None)
        else:
            ly.debug_output (_ ("Setting LilyPond's output to --verbose, implied by lilypond-book's setting"), True)
            global_options.process_cmd += " --verbose"

    if global_options.padding_mm:
        global_options.process_cmd += " -deps-box-padding=%f " % global_options.padding_mm

    global_options.process_cmd += " -dread-file-list -dno-strip-output-dir"

    if global_options.lily_output_dir:
        global_options.lily_output_dir = os.path.abspath(global_options.lily_output_dir)
        if not os.path.isdir (global_options.lily_output_dir):
            os.makedirs (global_options.lily_output_dir)
    else:
        global_options.lily_output_dir = os.path.abspath(global_options.output_dir)

    relative_output_dir = global_options.output_dir

    identify ()
    try:
        chunks = do_file (files[0])
    except BookSnippet.CompileError:
        exit (1)

    inputs = note_input_file ('')
    inputs.pop ()

    base_file_name = os.path.splitext (os.path.basename (files[0]))[0]
    dep_file = os.path.join (global_options.output_dir, base_file_name + '.dep')
    final_output_file = os.path.join (relative_output_dir,
                     base_file_name + global_options.formatter.default_extension)

    os.chdir (original_dir)
    file (dep_file, 'w').write ('%s: %s'
                                % (final_output_file, ' '.join (inputs)))

if __name__ == '__main__':
    main ()
