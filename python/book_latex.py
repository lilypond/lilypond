# -*- coding: utf-8 -*-

import re
import tempfile
import os
import subprocess
import book_base as BookBase
from book_snippets import *
import lilylib as ly
global _;_=ly._

progress = ly.progress
warning = ly.warning
error = ly.error

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
\input %(base)s-systems.tex
\ifx\postLilyPondExample \undefined
\else
  \expandafter\postLilyPondExample
\fi
}''',

    PRINTFILENAME: '''\\texttt{%(filename)s}
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
    m = re.search (r'''(?P<preamble>\\begin\s*{document})''', source)
    if m == None:
        warning (_ ("cannot find \\begin{document} in LaTeX document"))

        ## what's a sensible default?
        return 550.0

    preamble = source[:m.start (0)]
    latex_document = LATEX_INSPECTION_DOCUMENT % {'preamble': preamble}

    (handle, tmpfile) = tempfile.mkstemp('.tex')
    logfile = os.path.splitext (tmpfile)[0] + '.log'
    logfile = os.path.split (logfile)[1]

    tmp_handle = os.fdopen (handle,'w')
    tmp_handle.write (latex_document)
    tmp_handle.close ()

    progress (_ ("Running `%s' on file `%s' to detect default page settings.\n")
              % (global_options.latex_program, tmpfile));
    cmd = '%s %s' % (global_options.latex_program, tmpfile);
    proc = subprocess.Popen (cmd,
        universal_newlines=True, shell=True,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE);
    if proc.returncode != 0:
        warning (_ ("Unable to auto-detect default page settings:\n%s")
                 % proc.communicate ()[1]);
    os.unlink (tmpfile)
    parameter_string = ""
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

    textwidth = 0
    m = re.search ('textwidth=([0-9.]+)pt', parameter_string)
    if m:
        textwidth = float (m.group (1))
        if columns:
            textwidth = (textwidth - columnsep) / columns

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
            return os.popen ('kpsewhich ' + input_filename).read()[:-1]
        else:
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
        rep['base'] = basename
        str += self.output_print_filename (basename, snippet)
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
