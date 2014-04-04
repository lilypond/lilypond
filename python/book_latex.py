# -*- coding: utf-8 -*-

import re
import tempfile
import os
import sys
import subprocess
import book_base as BookBase
from book_snippets import *
import lilylib as ly
global _;_=ly._

progress = ly.progress
warning = ly.warning
error = ly.error
debug = ly.debug_output

# Recognize special sequences in the input.
#
#   (?P<name>regex) -- Assign result of REGEX to NAME.
#   *? -- Match non-greedily.
#   (?!...) -- Match if `...' doesn't match next (without consuming
#              the string).
#
#   (?m) -- Multiline regex: Make ^ and $ match at each line.
#   (?s) -- Make the dot match all characters including newline.
#   (?x) -- Ignore whitespace in patterns.
# Possible keys are:
#     'multiline_comment', 'verbatim', 'lilypond_block', 'singleline_comment',
#     'lilypond_file', 'include', 'lilypond', 'lilypondversion'
Latex_snippet_res = {
    'include':
         r'''(?smx)
          ^[^%\n]*?
          (?P<match>
          \\input\s*{
           (?P<filename>\S+?)
          })''',

    'lilypond':
         r'''(?smx)
          ^[^%\n]*?
          (?P<match>
          \\lilypond\s*(
          \[
           \s*(?P<options>.*?)\s*
          \])?\s*{
           (?P<code>.*?)
          })''',

    'lilypond_block':
         r'''(?smx)
          ^[^%\n]*?
          (?P<match>
          \\begin\s*(?P<env>{lilypond}\s*)?(
          \[
           \s*(?P<options>.*?)\s*
          \])?(?(env)|\s*{lilypond})
           (?P<code>.*?)
          ^[^%\n]*?
          \\end\s*{lilypond})''',

    'lilypond_file':
         r'''(?smx)
          ^[^%\n]*?
          (?P<match>
          \\lilypondfile\s*(
          \[
           \s*(?P<options>.*?)\s*
          \])?\s*\{
           (?P<filename>\S+?)
          })''',

    'musicxml_file':
         r'''(?smx)
          ^[^%\n]*?
          (?P<match>
          \\musicxmlfile\s*(
          \[
           \s*(?P<options>.*?)\s*
          \])?\s*\{
           (?P<filename>\S+?)
          })''',

    'singleline_comment':
         r'''(?mx)
          ^.*?
          (?P<match>
           (?P<code>
           %.*$\n+))''',

    'verb':
         r'''(?mx)
          ^[^%\n]*?
          (?P<match>
           (?P<code>
           \\verb(?P<del>.)
            .*?
           (?P=del)))''',

    'verbatim':
         r'''(?msx)
          ^[^%\n]*?
          (?P<match>
           (?P<code>
           \\begin\s*{verbatim}
            .*?
           \\end\s*{verbatim}))''',

    'lilypondversion':
         r'''(?smx)
          (?P<match>
          \\lilypondversion)[^a-zA-Z]''',
}

Latex_output = {
    FILTER: r'''\begin{lilypond}[%(options)s]
%(code)s
\end{lilypond}''',

    OUTPUT: r'''{%%
\parindent 0pt
\noindent
\ifx\preLilyPondExample \undefined
\else
  \expandafter\preLilyPondExample
\fi
\def\lilypondbook{}%%
\input{%(base)s-systems.tex}
\ifx\postLilyPondExample \undefined
\else
  \expandafter\postLilyPondExample
\fi
}''',

    PRINTFILENAME: r'''\texttt{%(filename)s}
\linebreak
''',

    QUOTE: r'''\begin{quote}
%(str)s
\end{quote}''',

    VERBATIM: r'''\noindent
\begin{verbatim}%(verb)s\end{verbatim}
''',

    VERSION: r'''%(program_version)s''',
}





###
# Retrieve dimensions from LaTeX
LATEX_INSPECTION_DOCUMENT = r'''
\nonstopmode
%(preamble)s
\begin{document}
\typeout{textwidth=\the\textwidth}
\typeout{columnsep=\the\columnsep}
\makeatletter\if@twocolumn\typeout{columns=2}\fi\makeatother
\end{document}
'''

# Do we need anything else besides `textwidth'?
def get_latex_textwidth (source, global_options):
    # default value
    textwidth = 550.0

    m = re.search (r'''(?P<preamble>\\begin\s*{document})''', source)
    if m == None:
        warning (_ ("cannot find \\begin{document} in LaTeX document"))
        return textwidth

    preamble = source[:m.start (0)]
    latex_document = LATEX_INSPECTION_DOCUMENT % {'preamble': preamble}

    (handle, tmpfile) = tempfile.mkstemp('.tex')
    tmpfileroot = os.path.splitext (tmpfile)[0]
    tmpfileroot = os.path.split (tmpfileroot)[1]
    auxfile = tmpfileroot + '.aux'
    logfile = tmpfileroot + '.log'

    tmp_handle = os.fdopen (handle,'w')
    tmp_handle.write (latex_document)
    tmp_handle.close ()

    progress (_ ("Running `%s' on file `%s' to detect default page settings.\n")
              % (global_options.latex_program, tmpfile))
    cmd = '%s %s' % (global_options.latex_program, tmpfile)
    debug ("Executing: %s\n" % cmd)
    run_env = os.environ.copy()
    run_env['LC_ALL'] = 'C'
    run_env['TEXINPUTS'] = '%s:%s' % \
                           (global_options.input_dir, run_env.get('TEXINPUTS',""))

    ### unknown why this is necessary
    universal_newlines = True
    if sys.platform == 'mingw32':
        universal_newlines = False
        ### use os.system to avoid weird sleep() problems on
        ### GUB's python 2.4.2 on mingw
        # make file to write to
        output_dir = tempfile.mkdtemp()
        output_filename = os.path.join(output_dir, 'output.txt')
        # call command
        cmd += " > %s" % output_filename
        oldtexinputs = os.environ.get ('TEXINPUTS')
        os.environ['TEXINPUTS'] = run_env['TEXINPUTS']
        returncode = os.system(cmd)
        if oldtexinputs:
            os.environ['TEXINPUTS'] = oldtexinputs
        else:
            del os.environ['TEXINPUTS']
        parameter_string = open(output_filename).read()
        if returncode != 0:
            warning (_ ("Unable to auto-detect default settings:\n"))
        # clean up
        os.remove(output_filename)
        os.rmdir(output_dir)
    else:
        proc = subprocess.Popen (cmd,
            env=run_env,
            universal_newlines=universal_newlines,
            shell=True,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        (parameter_string, error_string) = proc.communicate ()
        if proc.returncode != 0:
            warning (_ ("Unable to auto-detect default settings:\n%s")
                    % error_string)
    os.unlink (tmpfile)
    if os.path.exists (auxfile):
        os.unlink (auxfile)
    if os.path.exists (logfile):
        parameter_string = file (logfile).read()
        os.unlink (logfile)

    columns = 0
    m = re.search ('columns=([0-9.]+)', parameter_string)
    if m:
        columns = int (m.group (1))

    columnsep = 0
    m = re.search ('columnsep=([0-9.]+)pt', parameter_string)
    if m:
        columnsep = float (m.group (1))

    m = re.search ('textwidth=([0-9.]+)pt', parameter_string)
    if m:
        textwidth = float (m.group (1))
    else:
        warning (_ ("cannot detect textwidth from LaTeX"))
        return textwidth

    debug ('Detected values:')
    debug ('  columns = %s' % columns)
    debug ('  columnsep = %s' % columnsep)
    debug ('  textwidth = %s' % textwidth)

    if m and columns:
        textwidth = (textwidth - columnsep) / columns
        debug ('Adjusted value:')
        debug ('  textwidth = %s' % textwidth)

    return textwidth


def modify_preamble (chunk):
    str = chunk.replacement_text ()
    if (re.search (r"\\begin *{document}", str)
      and not re.search ("{graphic[sx]", str)):
        str = re.sub (r"\\begin{document}",
               r"\\usepackage{graphics}" + '\n'
               + r"\\begin{document}",
               str)
        chunk.override_text = str






class BookLatexOutputFormat (BookBase.BookOutputFormat):
    def __init__ (self):
        BookBase.BookOutputFormat.__init__ (self)
        self.format = "latex"
        self.default_extension = ".tex"
        self.snippet_res = Latex_snippet_res
        self.output = Latex_output
        self.handled_extensions = ['.latex', '.lytex', '.tex']
        self.image_formats = "ps"
        self.snippet_option_separator = '\s*,\s*'

    def process_options (self, global_options):
        self.process_options_pdfnotdefault (global_options)

    def get_line_width (self, source):
        textwidth = get_latex_textwidth (source, self.global_options)
        return '%.0f\\pt' % textwidth

    def input_fullname (self, input_filename):
        # Use kpsewhich if available, otherwise fall back to the default:
        if ly.search_exe_path ('kpsewhich'):
            trial = os.popen ('kpsewhich ' + input_filename).read()[:-1]
            if trial:
                return trial
        return BookBase.BookOutputFormat.input_fullname (self, input_filename)

    def process_chunks (self, chunks):
        for c in chunks:
            if (c.is_plain () and
              re.search (r"\\begin *{document}", c.replacement_text())):
                modify_preamble (c)
                break
        return chunks

    def snippet_output (self, basename, snippet):
        str = ''
        rep = snippet.get_replacements ();
        rep['base'] = basename.replace ('\\', '/')
        rep['filename'] = os.path.basename (snippet.filename).replace ('\\', '/')
        rep['ext'] = snippet.ext
        if PRINTFILENAME in snippet.option_dict:
            str += self.output[PRINTFILENAME] % rep
        if VERBATIM in snippet.option_dict:
            rep['verb'] = snippet.verb_ly ()
            str += self.output[VERBATIM] % rep

        str += self.output[OUTPUT] % rep

        ## todo: maintain breaks
        if 0:
            breaks = snippet.ly ().count ("\n")
            str += "".ljust (breaks, "\n").replace ("\n","%\n")

        if QUOTE in snippet.option_dict:
            str = self.output[QUOTE] % {'str': str}
        return str




BookBase.register_format (BookLatexOutputFormat ());
