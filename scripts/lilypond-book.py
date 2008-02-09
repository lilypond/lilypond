#!@TARGET_PYTHON@

'''
Example usage:

test:
  lilypond-book --filter="tr '[a-z]' '[A-Z]'" BOOK

convert-ly on book:
  lilypond-book --filter="convert-ly --no-version --from=1.6.11 -" BOOK

classic lilypond-book:
  lilypond-book --process="lilypond" BOOK.tely

TODO:

  *  this script is too complex. Modularize.
  
  *  ly-options: intertext?
  *  --line-width?
  *  eps in latex / eps by lilypond -b ps?
  *  check latex parameters, twocolumn, multicolumn?
  *  use --png --ps --pdf for making images?

  *  Converting from lilypond-book source, substitute:
   @mbinclude foo.itely -> @include foo.itely
   \mbinput -> \input

'''

import stat
import string
import tempfile
import commands
import os
import sys
import re
import md5

"""
@relocate-preamble@
"""

import lilylib as ly
import fontextract
global _;_=ly._


# Lilylib globals.
program_version = '@TOPLEVEL_VERSION@'
program_name = os.path.basename (sys.argv[0])

original_dir = os.getcwd ()
backend = 'ps'

help_summary = (
_ ("Process LilyPond snippets in hybrid HTML, LaTeX, texinfo or DocBook document.")
+ '\n\n'
+ _ ("Examples:")
+ '''
 lilypond-book --filter="tr '[a-z]' '[A-Z]'" %(BOOK)s
 lilypond-book --filter="convert-ly --no-version --from=2.0.0 -" %(BOOK)s
 lilypond-book --process='lilypond -I include' %(BOOK)s
''' % {'BOOK': _ ("BOOK")})

authors = ('Jan Nieuwenhuizen <janneke@gnu.org>',
      'Han-Wen Nienhuys <hanwen@xs4all.nl>')

################################################################
def exit (i):
    if global_options.verbose:
        raise _ ('Exiting (%d)...') % i
    else:
        sys.exit (i)

def identify ():
    ly.encoded_write (sys.stdout, '%s (GNU LilyPond) %s\n' % (program_name, program_version))

progress = ly.progress

def warning (s):
    ly.stderr_write (program_name + ": " + _ ("warning: %s") % s + '\n')

def error (s):
    ly.stderr_write (program_name + ": " + _ ("error: %s") % s + '\n')

def ps_page_count (ps_name):
    header = open (ps_name).read (1024)
    m = re.search ('\n%%Pages: ([0-9]+)', header)
    if m:
        return string.atoi (m.group (1))
    return 0

def warranty ():
    identify ()
    ly.encoded_write (sys.stdout, '''
%s

%s

%s
%s
''' % ( _ ('Copyright (c) %s by') % '2001--2007',
    ' '.join (authors),
   _ ("Distributed under terms of the GNU General Public License."),
   _ ("It comes with NO WARRANTY.")))

def get_option_parser ():
    p = ly.get_option_parser (usage=_ ("%s [OPTION]... FILE") % 'lilypond-book',
                              version="@TOPLEVEL_VERSION@",
                              description=help_summary)

    p.add_option ('-F', '--filter', metavar=_ ("FILTER"),
                  action="store",
                  dest="filter_cmd",
                  help=_ ("pipe snippets through FILTER [convert-ly -n -]"),
                  default=None)
    p.add_option ('-f', '--format',
                  help=_ ("use output format FORMAT (texi [default], texi-html, latex, html, docbook)"),
                  action='store')
    
    p.add_option ("-I", '--include', help=_ ("add DIR to include path"),
                  metavar=_ ("DIR"),
                  action='append', dest='include_path',
                  default=[os.path.abspath (os.getcwd ())])

    p.add_option ('--left-padding', 
                  metavar=_ ("PAD"),
                  dest="padding_mm",
                  help=_ ("pad left side of music to align music inspite of uneven bar numbers (in mm)"),
                  type="float",
                  default=3.0)
    
    p.add_option ("-o", '--output', help=_ ("write output to DIR"),
                  metavar=_ ("DIR"),
                  action='store', dest='output_name',
                  default='')
    
    p.add_option ('-P', '--process', metavar=_ ("COMMAND"),
                  help = _ ("process ly_files using COMMAND FILE..."),
                  action='store', 
                  dest='process_cmd', default='lilypond -dbackend=eps')
    p.add_option ('--pdf',
                  action="store_true",
                  dest="create_pdf",
                  help=_ ("create PDF files for use with PDFTeX"),
                  default=False)
    p.add_option ('', '--psfonts', action="store_true", dest="psfonts",
                  help=_ ('''extract all PostScript fonts into INPUT.psfonts for LaTeX
must use this with dvips -h INPUT.psfonts'''),
                  default=None)
    p.add_option ('-V', '--verbose', help=_ ("be verbose"),
                  action="store_true",
                  default=False,
                  dest="verbose")
    p.add_option ('-w', '--warranty',
                  help=_ ("show warranty and copyright"),
                  action='store_true')
    p.add_option_group (_ ('Bugs'),
                        description=(_ ("Report bugs via")
                                     + ''' http://post.gmane.org/post.php'''
                                     '''?group=gmane.comp.gnu.lilypond.bugs\n'''))
    return p

lilypond_binary = os.path.join ('@bindir@', 'lilypond')

# Only use installed binary when we are installed too.
if '@bindir@' == ('@' + 'bindir@') or not os.path.exists (lilypond_binary):
    lilypond_binary = 'lilypond'

global_options = None


default_ly_options = { 'alt': "[image of music]" }

#
# Is this pythonic?  Personally, I find this rather #define-nesque. --hwn
#
AFTER = 'after'
BEFORE = 'before'
DOCBOOK = 'docbook'
EXAMPLEINDENT = 'exampleindent'
FILTER = 'filter'
FRAGMENT = 'fragment'
HTML = 'html'
INDENT = 'indent'
LATEX = 'latex'
LAYOUT = 'layout'
LINE_WIDTH = 'line-width'
LILYQUOTE = 'lilyquote'
NOFRAGMENT = 'nofragment'
NOINDENT = 'noindent'
NOQUOTE = 'noquote'
NOTES = 'body'
NOTIME = 'notime'
OUTPUT = 'output'
OUTPUTIMAGE = 'outputimage'
PACKED = 'packed'
PAPER = 'paper'
PREAMBLE = 'preamble'
PRINTFILENAME = 'printfilename'
QUOTE = 'quote'
RAGGED_RIGHT = 'ragged-right'
RELATIVE = 'relative'
STAFFSIZE = 'staffsize'
TEXIDOC = 'texidoc'
TEXINFO = 'texinfo'
VERBATIM = 'verbatim'
FONTLOAD = 'fontload'
FILENAME = 'filename'
ALT = 'alt'


# NOTIME has no opposite so it isn't part of this dictionary.
# NOQUOTE is used internally only.
no_options = {
    NOFRAGMENT: FRAGMENT,
    NOINDENT: INDENT,
}


# Recognize special sequences in the input.
#
#   (?P<name>regex) -- Assign result of REGEX to NAME.
#   *? -- Match non-greedily.
#   (?m) -- Multiline regex: Make ^ and $ match at each line.
#   (?s) -- Make the dot match all characters including newline.
#   (?x) -- Ignore whitespace in patterns.
no_match = 'a\ba'
snippet_res = {
 ##
    DOCBOOK: {
        'include':
         no_match,

        'lilypond':
         r'''(?smx)
          (?P<match>
          <(?P<inline>(inline)?)mediaobject>\s*<textobject.*?>\s*<programlisting\s+language="lilypond".*?(role="(?P<options>.*?)")?>(?P<code>.*?)</programlisting\s*>\s*</textobject\s*>\s*</(inline)?mediaobject>)''',

        'lilypond_block':
         r'''(?smx)
          (?P<match>
          <(?P<inline>(inline)?)mediaobject>\s*<textobject.*?>\s*<programlisting\s+language="lilypond".*?(role="(?P<options>.*?)")?>(?P<code>.*?)</programlisting\s*>\s*</textobject\s*>\s*</(inline)?mediaobject>)''',

        'lilypond_file':
         r'''(?smx)
          (?P<match>
          <(?P<inline>(inline)?)mediaobject>\s*<imageobject.*?>\s*<imagedata\s+fileref="(?P<filename>.*?\.ly)"\s*(role="(?P<options>.*?)")?\s*(/>|>\s*</imagedata>)\s*</imageobject>\s*</(inline)?mediaobject>)''',

        'multiline_comment':
         r'''(?smx)
          (?P<match>
          \s*(?!@c\s+)
          (?P<code><!--\s.*?!-->)
          \s)''',

        'singleline_comment':
         no_match,

        'verb':
         no_match,

        'verbatim':
	no_match,
    	
    }, 
    ##
    HTML: {
        'include':
         no_match,

        'lilypond':
         r'''(?mx)
          (?P<match>
          <lilypond
           (\s*(?P<options>.*?)\s*:)?\s*
           (?P<code>.*?)
          />)''',

        'lilypond_block':
         r'''(?msx)
          (?P<match>
          <lilypond
           \s*(?P<options>.*?)\s*
          >
          (?P<code>.*?)
          </lilypond>)''',

        'lilypond_file':
         r'''(?mx)
          (?P<match>
          <lilypondfile
           \s*(?P<options>.*?)\s*
          >
          \s*(?P<filename>.*?)\s*
          </lilypondfile>)''',

        'multiline_comment':
         r'''(?smx)
          (?P<match>
          \s*(?!@c\s+)
          (?P<code><!--\s.*?!-->)
          \s)''',

        'singleline_comment':
         no_match,

        'verb':
         r'''(?x)
          (?P<match>
           (?P<code><pre>.*?</pre>))''',

        'verbatim':
         r'''(?x)
          (?s)
          (?P<match>
           (?P<code><pre>\s.*?</pre>\s))''',
    },

    ##
    LATEX: {
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
          \\begin\s*(
          \[
           \s*(?P<options>.*?)\s*
          \])?\s*{lilypond}
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

        'multiline_comment':
         no_match,

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
    },

    ##
    TEXINFO: {
        'include':
         r'''(?mx)
          ^(?P<match>
          @include\s+
           (?P<filename>\S+))''',

        'lilypond':
         r'''(?smx)
          ^[^\n]*?(?!@c\s+)[^\n]*?
          (?P<match>
          @lilypond\s*(
          \[
           \s*(?P<options>.*?)\s*
          \])?\s*{
           (?P<code>.*?)
          })''',

        'lilypond_block':
         r'''(?msx)
          ^(?P<match>
          @lilypond\s*(
          \[
           \s*(?P<options>.*?)\s*
          \])?\s+?
          ^(?P<code>.*?)
          ^@end\s+lilypond)\s''',

        'lilypond_file':
         r'''(?mx)
          ^(?P<match>
          @lilypondfile\s*(
          \[
           \s*(?P<options>.*?)\s*
          \])?\s*{
           (?P<filename>\S+)
          })''',

        'multiline_comment':
         r'''(?smx)
          ^(?P<match>
           (?P<code>
           @ignore\s
            .*?
           @end\s+ignore))\s''',

        'singleline_comment':
         r'''(?mx)
          ^.*
          (?P<match>
           (?P<code>
           @c([ \t][^\n]*|)\n))''',

    # Don't do this: It interferes with @code{@{}.
    #        'verb': r'''(?P<code>@code{.*?})''',

        'verbatim':
         r'''(?sx)
          (?P<match>
           (?P<code>
           @example
            \s.*?
           @end\s+example\s))''',
    },
}




format_res = {
    DOCBOOK: {        
    	'intertext': r',?\s*intertext=\".*?\"',
        'option_sep': '\s*',
    }, 
    HTML: {
        'intertext': r',?\s*intertext=\".*?\"',
        'option_sep': '\s*',
    },

    LATEX: {
        'intertext': r',?\s*intertext=\".*?\"',
        'option_sep': '\s*,\s*',
    },

    TEXINFO: {
        'intertext': r',?\s*intertext=\".*?\"',
        'option_sep': '\s*,\s*',
    },
}

# Options without a pattern in ly_options.
simple_options = [
    EXAMPLEINDENT,
    FRAGMENT,
    NOFRAGMENT,
    NOINDENT,
    PRINTFILENAME,
    TEXIDOC,
    VERBATIM,
    FONTLOAD,
    FILENAME,
    ALT
]

ly_options = {
    ##
    NOTES: {
        RELATIVE: r'''\relative c%(relative_quotes)s''',
    },

    ##
    PAPER: {
        INDENT: r'''indent = %(indent)s''',

        LINE_WIDTH: r'''line-width = %(line-width)s''',

        QUOTE: r'''line-width = %(line-width)s - 2.0 * %(exampleindent)s''',

        LILYQUOTE: r'''line-width = %(line-width)s - 2.0 * %(exampleindent)s''',

        RAGGED_RIGHT: r'''ragged-right = ##t''',

        PACKED: r'''packed = ##t''',
    },

    ##
    LAYOUT: {
        NOTIME: r'''
 \context {
  \Score
  timing = ##f
 }
 \context {
  \Staff
  \remove Time_signature_engraver
 }''',
    },

    ##
    PREAMBLE: {
        STAFFSIZE: r'''#(set-global-staff-size %(staffsize)s)''',
    },
}

output = {
    ##
    DOCBOOK: {                 
        FILTER: r'''<mediaobject><textobject><programlisting language="lilypond" role="%(options)s">%(code)s</programlisting></textobject></mediaobject>''', 
    
        OUTPUT: r'''
        <imageobject role="latex">
		<imagedata fileref="%(base)s.pdf" format="PDF"/>
		</imageobject>
		<imageobject role="html">
		<imagedata fileref="%(base)s.png" format="PNG"/></imageobject>''',
    
        VERBATIM: r'''<programlisting>%(verb)s</programlisting>''',
    
        PRINTFILENAME: '<textobject><simpara><ulink url="%(base)s.ly"><filename>%(filename)s</filename></ulink></simpara></textobject>'
    },
    ##
    HTML: {
        FILTER: r'''<lilypond %(options)s>
%(code)s
</lilypond>
''',

        AFTER: r'''
 </a>
</p>''',

        BEFORE: r'''<p>
 <a href="%(base)s.ly">''',

        OUTPUT: r'''
  <img align="center" valign="center"
    border="0" src="%(image)s" alt="%(alt)s">''',

        PRINTFILENAME: '<p><tt><a href="%(base)s.ly">%(filename)s</a></tt></p>',

        QUOTE: r'''<blockquote>
%(str)s
</blockquote>
''',

        VERBATIM: r'''<pre>
%(verb)s</pre>''',
    },

    ##
    LATEX: {
        OUTPUT: r'''{%%
\parindent 0pt%%
\ifx\preLilyPondExample \undefined%%
 \relax%%
\else%%
 \preLilyPondExample%%
\fi%%
\def\lilypondbook{}%%
\input %(base)s-systems.tex%%
\ifx\postLilyPondExample \undefined%%
 \relax%%
\else%%
 \postLilyPondExample%%
\fi%%
}''',

        PRINTFILENAME: '''\\texttt{%(filename)s}
    ''',

        QUOTE: r'''\begin{quotation}%(str)s
\end{quotation}''',

        VERBATIM: r'''\noindent
\begin{verbatim}%(verb)s\end{verbatim}''',

        FILTER: r'''\begin{lilypond}[%(options)s]
%(code)s
\end{lilypond}''',
    },

    ##
    TEXINFO: {
        FILTER: r'''@lilypond[%(options)s]
%(code)s
@lilypond''',

        OUTPUT: r'''
@iftex
@include %(base)s-systems.texi
@end iftex
''',

        OUTPUTIMAGE: r'''@noindent
@ifinfo
@image{%(base)s,,,%(alt)s,%(ext)s}
@end ifinfo
@html
<p>
 <a href="%(base)s.ly">
  <img align="center" valign="center"
    border="0" src="%(image)s" alt="%(alt)s">
 </a>
</p>
@end html
''',

        PRINTFILENAME: '''
@html
<a href="%(base)s.ly">
@end html
@file{%(filename)s}
@html
</a>
@end html
    ''',

        QUOTE: r'''@quotation
%(str)s@end quotation
''',

        NOQUOTE: r'''@format
%(str)s@end format
''',

        VERBATIM: r'''@exampleindent 0
@verbatim
%(verb)s@end verbatim
''',
    },
}

#
# Maintain line numbers.
#

## TODO
if 0:
    for f in [HTML, LATEX]:
        for s in (QUOTE, VERBATIM):
            output[f][s] = output[f][s].replace("\n"," ")


PREAMBLE_LY = '''%%%% Generated by %(program_name)s
%%%% Options: [%(option_string)s]
\\include "lilypond-book-preamble.ly"


%% ****************************************************************
%% Start cut-&-pastable-section 
%% ****************************************************************

%(preamble_string)s

\paper {
  #(define dump-extents #t)
  %(font_dump_setting)s
  %(paper_string)s
  force-assignment = #""
  line-width = #(- line-width (* mm  %(padding_mm)f))
}

\layout {
  %(layout_string)s
}
'''

FRAGMENT_LY = r'''
%(notes_string)s
{


%% ****************************************************************
%% ly snippet contents follows:
%% ****************************************************************
%(code)s


%% ****************************************************************
%% end ly snippet
%% ****************************************************************
}
'''

FULL_LY = '''


%% ****************************************************************
%% ly snippet:
%% ****************************************************************
%(code)s


%% ****************************************************************
%% end ly snippet
%% ****************************************************************
'''

texinfo_line_widths = {
    '@afourpaper': '160\\mm',
    '@afourwide': '6.5\\in',
    '@afourlatex': '150\\mm',
    '@smallbook': '5\\in',
    '@letterpaper': '6\\in',
}

def classic_lilypond_book_compatibility (key, value):
    if key == 'singleline' and value == None:
        return (RAGGED_RIGHT, None)

    m = re.search ('relative\s*([-0-9])', key)
    if m:
        return ('relative', m.group (1))

    m = re.match ('([0-9]+)pt', key)
    if m:
        return ('staffsize', m.group (1))

    if key == 'indent' or key == 'line-width':
        m = re.match ('([-.0-9]+)(cm|in|mm|pt|staffspace)', value)
        if m:
            f = float (m.group (1))
            return (key, '%f\\%s' % (f, m.group (2)))

    return (None, None)

def find_file (name):
    for i in global_options.include_path:
        full = os.path.join (i, name)
        if os.path.exists (full):
            return full
        
    error (_ ("file not found: %s") % name + '\n')
    exit (1)
    return ''

def verbatim_html (s):
    return re.sub ('>', '&gt;',
           re.sub ('<', '&lt;',
               re.sub ('&', '&amp;', s)))

def split_options (option_string):
    if option_string:
        if global_options.format == HTML:
            options = re.findall('[\w\.-:]+(?:\s*=\s*(?:"[^"]*"|\'[^\']*\'|\S+))?',option_string)
            for i in range(len(options)):
                options[i] = re.sub('^([^=]+=\s*)(?P<q>["\'])(.*)(?P=q)','\g<1>\g<3>',options[i])
            return options
        else:
            return re.split (format_res[global_options.format]['option_sep'],
                    option_string)
    return []

def set_default_options (source):
    global default_ly_options
    if not default_ly_options.has_key (LINE_WIDTH):
        if global_options.format == LATEX:
            textwidth = get_latex_textwidth (source)
            default_ly_options[LINE_WIDTH] = \
             '''%.0f\\pt''' % textwidth
        elif global_options.format == TEXINFO:
            for (k, v) in texinfo_line_widths.items ():
                # FIXME: @layout is usually not in
                # chunk #0:
                #
                #  \input texinfo @c -*-texinfo-*-
                #
                # Bluntly search first K items of
                # source.
                # s = chunks[0].replacement_text ()
                if re.search (k, source[:1024]):
                    default_ly_options[LINE_WIDTH] = v
                    break

class Chunk:
    def replacement_text (self):
        return ''

    def filter_text (self):
        return self.replacement_text ()

    def ly_is_outdated (self):
        return 0

    def png_is_outdated (self):
        return 0

    def is_plain (self):
        return False
    
class Substring (Chunk):
    def __init__ (self, source, start, end, line_number):
        self.source = source
        self.start = start
        self.end = end
        self.line_number = line_number
        self.override_text = None
        
    def is_plain (self):
        return True

    def replacement_text (self):
        if self.override_text:
            return self.override_text
        else:
            return self.source[self.start:self.end]

class Snippet (Chunk):
    def __init__ (self, type, match, format, line_number):
        self.type = type
        self.match = match
        self.hash = 0
        self.option_dict = {}
        self.format = format
        self.line_number = line_number

    def replacement_text (self):
        return self.match.group ('match')

    def substring (self, s):
        return self.match.group (s)

    def __repr__ (self):
        return `self.__class__` + ' type = ' + self.type

class Include_snippet (Snippet):
    def processed_filename (self):
        f = self.substring ('filename')
        return os.path.splitext (f)[0] + format2ext[global_options.format]

    def replacement_text (self):
        s = self.match.group ('match')
        f = self.substring ('filename')

        return re.sub (f, self.processed_filename (), s)

class Lilypond_snippet (Snippet):
    def __init__ (self, type, match, format, line_number):
        Snippet.__init__ (self, type, match, format, line_number)
        os = match.group ('options')
        self.do_options (os, self.type)

    def verb_ly (self):
        return self.substring ('code')

    def ly (self):
        contents = self.substring ('code')
        return ('\\sourcefileline %d\n%s'
                % (self.line_number - 1, contents))

    def full_ly (self):
        s = self.ly ()
        if s:
            return self.compose_ly (s)
        return ''

    def do_options (self, option_string, type):
        self.option_dict = {}

        options = split_options (option_string)

        for i in options:
            if '=' in i:
                (key, value) = re.split ('\s*=\s*', i)
                self.option_dict[key] = value
            else:
                if i in no_options.keys ():
                    if no_options[i] in self.option_dict.keys ():
                        del self.option_dict[no_options[i]]
                else:
                    self.option_dict[i] = None

        has_line_width = self.option_dict.has_key (LINE_WIDTH)
        no_line_width_value = 0

        # If LINE_WIDTH is used without parameter, set it to default.
        if has_line_width and self.option_dict[LINE_WIDTH] == None:
            no_line_width_value = 1
            del self.option_dict[LINE_WIDTH]

        for i in default_ly_options.keys ():
            if i not in self.option_dict.keys ():
                self.option_dict[i] = default_ly_options[i]

        if not has_line_width:
            if type == 'lilypond' or FRAGMENT in self.option_dict.keys ():
                self.option_dict[RAGGED_RIGHT] = None

            if type == 'lilypond':
                if LINE_WIDTH in self.option_dict.keys ():
                    del self.option_dict[LINE_WIDTH]
            else:
                if RAGGED_RIGHT in self.option_dict.keys ():
                    if LINE_WIDTH in self.option_dict.keys ():
                        del self.option_dict[LINE_WIDTH]

            if QUOTE in self.option_dict.keys () or type == 'lilypond':
                if LINE_WIDTH in self.option_dict.keys ():
                    del self.option_dict[LINE_WIDTH]

        if not INDENT in self.option_dict.keys ():
            self.option_dict[INDENT] = '0\\mm'

        # The QUOTE pattern from ly_options only emits the `line-width'
        # keyword.
        if has_line_width and QUOTE in self.option_dict.keys ():
            if no_line_width_value:
                del self.option_dict[LINE_WIDTH]
            else:
                del self.option_dict[QUOTE]

    def compose_ly (self, code):
        if FRAGMENT in self.option_dict.keys ():
            body = FRAGMENT_LY
        else:
            body = FULL_LY

        # Defaults.
        relative = 1
        override = {}
        # The original concept of the `exampleindent' option is broken.
        # It is not possible to get a sane value for @exampleindent at all
        # without processing the document itself.  Saying
        #
        #   @exampleindent 0
        #   @example
        #   ...
        #   @end example
        #   @exampleindent 5
        #
        # causes ugly results with the DVI backend of texinfo since the
        # default value for @exampleindent isn't 5em but 0.4in (or a smaller
        # value).  Executing the above code changes the environment
        # indentation to an unknown value because we don't know the amount
        # of 1em in advance since it is font-dependent.  Modifying
        # @exampleindent in the middle of a document is simply not
        # supported within texinfo.
        #
        # As a consequence, the only function of @exampleindent is now to
        # specify the amount of indentation for the `quote' option.
        #
        # To set @exampleindent locally to zero, we use the @format
        # environment for non-quoted snippets.
        override[EXAMPLEINDENT] = r'0.4\in'
        override[LINE_WIDTH] = texinfo_line_widths['@smallbook']
        override.update (default_ly_options)

        option_list = []
        for (key, value) in self.option_dict.items ():
            if value == None:
                option_list.append (key)
            else:
                option_list.append (key + '=' + value)
        option_string = string.join (option_list, ',')

        compose_dict = {}
        compose_types = [NOTES, PREAMBLE, LAYOUT, PAPER]
        for a in compose_types:
            compose_dict[a] = []

        for (key, value) in self.option_dict.items ():
            (c_key, c_value) = \
             classic_lilypond_book_compatibility (key, value)
            if c_key:
                if c_value:
                    warning \
                     (_ ("deprecated ly-option used: %s=%s" \
                      % (key, value)))
                    warning \
                     (_ ("compatibility mode translation: %s=%s" \
                      % (c_key, c_value)))
                else:
                    warning \
                     (_ ("deprecated ly-option used: %s" \
                      % key))
                    warning \
                     (_ ("compatibility mode translation: %s" \
                      % c_key))

                (key, value) = (c_key, c_value)

            if value:
                override[key] = value
            else:
                if not override.has_key (key):
                    override[key] = None

            found = 0
            for type in compose_types:
                if ly_options[type].has_key (key):
                    compose_dict[type].append (ly_options[type][key])
                    found = 1
                    break

            if not found and key not in simple_options:
                warning (_ ("ignoring unknown ly option: %s") % key)

        # URGS
        if RELATIVE in override.keys () and override[RELATIVE]:
            relative = int (override[RELATIVE])

        relative_quotes = ''

        # 1 = central C
        if relative < 0:
            relative_quotes += ',' * (- relative)
        elif relative > 0:
            relative_quotes += "'" * relative

        paper_string = string.join (compose_dict[PAPER],
                      '\n  ') % override
        layout_string = string.join (compose_dict[LAYOUT],
                      '\n  ') % override
        notes_string = string.join (compose_dict[NOTES],
                      '\n  ') % vars ()
        preamble_string = string.join (compose_dict[PREAMBLE],
                       '\n  ') % override
        padding_mm = global_options.padding_mm
        font_dump_setting = ''
        if FONTLOAD in self.option_dict:
            font_dump_setting = '#(define-public force-eps-font-include #t)\n'

        d = globals().copy()
        d.update (locals())
        return (PREAMBLE_LY + body) % d

    def get_hash (self):
        if not self.hash:
            hash = md5.md5 (self.relevant_contents (self.full_ly ()))

            ## let's not create too long names.
            self.hash = hash.hexdigest ()[:10]
            
        return self.hash

    def basename (self):
        if FILENAME in self.option_dict:
            return self.option_dict[FILENAME]
        if global_options.use_hash:
            return 'lily-%s' % self.get_hash ()
        raise 'to be done'

    def write_ly (self):
        outf = open (self.basename () + '.ly', 'w')
        outf.write (self.full_ly ())
        open (self.basename () + '.txt', 'w').write ('image of music')

    def relevant_contents (self, ly):
        return re.sub (r'\\(version|sourcefileline|sourcefilename)[^\n]*\n', '', ly)
             
    def ly_is_outdated (self):
        base = self.basename ()
        ly_file = base + '.ly'
        tex_file = base + '.tex'
        eps_file = base + '.eps'
        systems_file = base + '-systems.tex'

        if (os.path.exists (ly_file)
            and os.path.exists (systems_file)
            and os.stat (systems_file)[stat.ST_SIZE]
            and re.match ('% eof', open (systems_file).readlines ()[-1])
            and (global_options.use_hash or FILENAME in self.option_dict)
            and (self.relevant_contents (self.full_ly ())
                 == self.relevant_contents (open (ly_file).read ()))):
            return None

        return self

    def png_is_outdated (self):
        base = self.basename ()
        # FIXME: refactor stupid OK stuff
        ok = not self.ly_is_outdated ()
        if global_options.format in (HTML, TEXINFO):
            ok = ok and os.path.exists (base + '.eps')

            page_count = 0
            if ok:
                page_count = ps_page_count (base + '.eps')

            if page_count <= 1:
                ok = ok and os.path.exists (base + '.png')
             
            elif page_count > 1:
                for a in range (1, page_count + 1):
                        ok = ok and os.path.exists (base + '-page%d.png' % a)
                
        return not ok
    
    def texstr_is_outdated (self):
        if backend == 'ps':
            return 0

        # FIXME: refactor stupid OK stuff
        base = self.basename ()
        ok = self.ly_is_outdated ()
        ok = ok and (os.path.exists (base + '.texstr'))
        return not ok

    def filter_text (self):
        code = self.substring ('code')
        s = run_filter (code)
        d = {
            'code': s,
            'options': self.match.group ('options')
        }
        # TODO
        return output[self.format][FILTER] % d

    def replacement_text (self):
        func = Lilypond_snippet.__dict__['output_' + self.format]
        return func (self)

    def get_images (self):
        base = self.basename ()
        # URGUGHUGHUGUGH
        single = '%(base)s.png' % vars ()
        multiple = '%(base)s-page1.png' % vars ()
        images = (single,)
        if os.path.exists (multiple) \
         and (not os.path.exists (single) \
            or (os.stat (multiple)[stat.ST_MTIME] \
              > os.stat (single)[stat.ST_MTIME])):
            count = ps_page_count ('%(base)s.eps' % vars ())
            images = ['%s-page%d.png' % (base, a) for a in range (1, count+1)]
            images = tuple (images)
        return images

    def output_docbook (self):
        str = ''
        base = self.basename ()
        for image in self.get_images ():
            (base, ext) = os.path.splitext (image)
            str += output[DOCBOOK][OUTPUT] % vars ()
	    str += self.output_print_filename (DOCBOOK)
            if (self.substring('inline') == 'inline'): 
                str = '<inlinemediaobject>' + str + '</inlinemediaobject>'
            else:
                str = '<mediaobject>' + str + '</mediaobject>'
        if VERBATIM in self.option_dict:
                verb = verbatim_html (self.verb_ly ())
                str = output[DOCBOOK][VERBATIM] % vars () + str
        return str
	
    def output_html (self):
        str = ''
        base = self.basename ()
        if global_options.format == HTML:
            str += self.output_print_filename (HTML)
            if VERBATIM in self.option_dict:
                verb = verbatim_html (self.verb_ly ())
                str += output[HTML][VERBATIM] % vars ()
            if QUOTE in self.option_dict:
                str = output[HTML][QUOTE] % vars ()

        str += output[HTML][BEFORE] % vars ()
        for image in self.get_images ():
            (base, ext) = os.path.splitext (image)
            alt = self.option_dict[ALT]
            str += output[HTML][OUTPUT] % vars ()
        str += output[HTML][AFTER] % vars ()
        return str

    def output_info (self):
        str = ''
        for image in self.get_images ():
            (base, ext) = os.path.splitext (image)

            # URG, makeinfo implicitly prepends dot to extension.
            # Specifying no extension is most robust.
            ext = ''
            alt = self.option_dict[ALT]
            str += output[TEXINFO][OUTPUTIMAGE] % vars ()

        base = self.basename ()
        str += output[global_options.format][OUTPUT] % vars ()
        return str

    def output_latex (self):
        str = ''
        base = self.basename ()
        if global_options.format == LATEX:
            str += self.output_print_filename (LATEX)
            if VERBATIM in self.option_dict:
                verb = self.verb_ly ()
                str += (output[LATEX][VERBATIM] % vars ())

        str += (output[LATEX][OUTPUT] % vars ())

        ## todo: maintain breaks
        if 0:
            breaks = self.ly ().count ("\n")
            str += "".ljust (breaks, "\n").replace ("\n","%\n")
        
        if QUOTE in self.option_dict:
            str = output[LATEX][QUOTE] % vars ()
        return str

    def output_print_filename (self, format):
        str = ''
        if PRINTFILENAME in self.option_dict:
            base = self.basename ()
            filename = self.substring ('filename')
            str = output[global_options.format][PRINTFILENAME] % vars ()

        return str

    def output_texinfo (self):
        str = ''
        if self.output_print_filename (TEXINFO):
            str += ('@html\n'
                + self.output_print_filename (HTML)
                + '\n@end html\n')
            str += ('@tex\n'
                + self.output_print_filename (LATEX)
                + '\n@end tex\n')
        base = self.basename ()
        if TEXIDOC in self.option_dict:
            texidoc = base + '.texidoc'
            if os.path.exists (texidoc):
                str += '@include %(texidoc)s\n\n' % vars ()

        substr = ''
        if VERBATIM in self.option_dict:
            verb = self.verb_ly ()
            substr += output[TEXINFO][VERBATIM] % vars ()
            if not QUOTE in self.option_dict:
                substr = output[TEXINFO][NOQUOTE] % {'str':substr}
        substr += self.output_info ()
        if LILYQUOTE in self.option_dict:
            substr = output[TEXINFO][QUOTE] % {'str':substr}
        str += substr

#                str += ('@ifinfo\n' + self.output_info () + '\n@end ifinfo\n')
#                str += ('@tex\n' + self.output_latex () + '\n@end tex\n')
#                str += ('@html\n' + self.output_html () + '\n@end html\n')

        if QUOTE in self.option_dict:
            str = output[TEXINFO][QUOTE] % vars ()

        # need par after image
        str += '\n'

        return str

re_begin_verbatim = re.compile (r'\s+%.*?begin verbatim.*\n*', re.M)
re_end_verbatim = re.compile (r'\s+%.*?end verbatim.*$', re.M)

class Lilypond_file_snippet (Lilypond_snippet):
    def __init__ (self, type, match, format, line_number):
        Lilypond_snippet.__init__ (self, type, match, format, line_number)
        self.contents = open (find_file (self.substring ('filename'))).read ()

    def verb_ly (self):
        s = self.contents
        s = re_begin_verbatim.split (s)[-1]
        s = re_end_verbatim.split (s)[0]
        return s

    def ly (self):
        name = self.substring ('filename')
        return ('\\sourcefilename \"%s\"\n\\sourcefileline 0\n%s'
                % (name, self.contents))


snippet_type_to_class = {
    'lilypond_file': Lilypond_file_snippet,
    'lilypond_block': Lilypond_snippet,
    'lilypond': Lilypond_snippet,
    'include': Include_snippet,
}

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

def find_toplevel_snippets (s, types):
    res = {}
    for i in types:
        res[i] = ly.re.compile (snippet_res[global_options.format][i])

    snippets = []
    index = 0
    found = dict ([(t, None) for t in types])

    line_starts = find_linestarts (s)
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
                
                m = res[type].search (s[index:endex])
                if not m:
                    continue

                cl = Snippet
                if snippet_type_to_class.has_key (type):
                    cl = snippet_type_to_class[type]


                start = index + m.start ('match')
                line_number = line_start_idx
                while (line_starts[line_number] < start):
                    line_number += 1

                line_number += 1
                snip = cl (type, m, global_options.format, line_number)

                found[type] = (start, snip)

            if found[type] \
             and (not first \
                or found[type][0] < found[first][0]):
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
            snippets.append (Substring (s, index, len (s), line_start_idx))
            break

        while (start > line_starts[line_start_idx+1]):
            line_start_idx += 1

        (start, snip) = found[first]
        snippets.append (Substring (s, index, start, line_start_idx + 1))
        snippets.append (snip)
        found[first] = None
        index = start + len (snip.match.group ('match'))

    return snippets

def filter_pipe (input, cmd):
    if global_options.verbose:
        progress (_ ("Opening filter `%s'") % cmd)

    (stdin, stdout, stderr) = os.popen3 (cmd)
    stdin.write (input)
    status = stdin.close ()

    if not status:
        status = 0
        output = stdout.read ()
        status = stdout.close ()
        error = stderr.read ()

    if not status:
        status = 0
    signal = 0x0f & status
    if status or (not output and error):
        exit_status = status >> 8
        error (_ ("`%s' failed (%d)") % (cmd, exit_status))
        error (_ ("The error log is as follows:"))
        ly.stderr_write (error)
        ly.stderr_write (stderr.read ())
        exit (status)

    if global_options.verbose:
        progress ('\n')

    return output

def run_filter (s):
    return filter_pipe (s, global_options.filter_cmd)

def is_derived_class (cl, baseclass):
    if cl == baseclass:
        return 1
    for b in cl.__bases__:
        if is_derived_class (b, baseclass):
            return 1
    return 0

def process_snippets (cmd, ly_snippets, texstr_snippets, png_snippets):
    ly_names = filter (lambda x: x,
                       map (Lilypond_snippet.basename, ly_snippets))
    texstr_names = filter (lambda x: x,
                           map (Lilypond_snippet.basename, texstr_snippets))
    
    png_names = filter (lambda x: x,
                        map (Lilypond_snippet.basename, png_snippets))

    status = 0
    def my_system (cmd):
        status = ly.system (cmd,
                            be_verbose=global_options.verbose, 
                            progress_p=1)

    if global_options.format in (HTML, TEXINFO) and '--formats' not in cmd:
        cmd += ' --formats=png '
    elif global_options.format in (DOCBOOK) and '--formats' not in cmd:
        cmd += ' --formats=png,pdf '

        
    # UGH
    # the --process=CMD switch is a bad idea
    # it is too generic for lilypond-book.
    if texstr_names:
        my_system (string.join ([cmd, '--backend texstr',
                                 'snippet-map.ly'] + texstr_names))
        for l in texstr_names:
            my_system ('latex %s.texstr' % l)

    if ly_names:
        open ('snippet-names', 'wb').write ('\n'.join (['snippet-map.ly']
                                                      + ly_names))
        
        my_system (string.join ([cmd, 'snippet-names']))


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
def get_latex_textwidth (source):
    m = re.search (r'''(?P<preamble>\\begin\s*{document})''', source)
    if m == None:
        warning (_ ("cannot find \\begin{document} in LaTeX document"))
        
        ## what's a sensible default?
        return 550.0
    
    preamble = source[:m.start (0)]
    latex_document = LATEX_INSPECTION_DOCUMENT % vars ()
    
    (handle, tmpfile) = tempfile.mkstemp('.tex')
    logfile = os.path.splitext (tmpfile)[0] + '.log'
    logfile = os.path.split (logfile)[1]

    tmp_handle = os.fdopen (handle,'w')
    tmp_handle.write (latex_document)
    tmp_handle.close ()
    
    ly.system ('latex %s' % tmpfile, be_verbose=global_options.verbose)
    parameter_string = open (logfile).read()
    
    os.unlink (tmpfile)
    os.unlink (logfile)

    columns = 0
    m = re.search ('columns=([0-9.]*)', parameter_string)
    if m:
        columns = int (m.group (1))

    columnsep = 0
    m = re.search ('columnsep=([0-9.]*)pt', parameter_string)
    if m:
        columnsep = float (m.group (1))

    textwidth = 0
    m = re.search ('textwidth=([0-9.]*)pt', parameter_string)
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
        
    

ext2format = {
    '.html': HTML,
    '.itely': TEXINFO,
    '.latex': LATEX,
    '.lytex': LATEX,
    '.tely': TEXINFO,
    '.tex': LATEX,
    '.texi': TEXINFO,
    '.texinfo': TEXINFO,
    '.xml': HTML,
    '.lyxml': DOCBOOK
}

format2ext = {
    HTML: '.html',
    # TEXINFO: '.texinfo',
    TEXINFO: '.texi',
    LATEX: '.tex',
    DOCBOOK: '.xml'
}

class Compile_error:
    pass

def write_file_map (lys, name):
    snippet_map = open ('snippet-map.ly', 'w')
    snippet_map.write ("""
#(define version-seen #t)
#(define output-empty-score-list #f)
#(ly:add-file-name-alist '(
""")
    for ly in lys:
        snippet_map.write ('("%s.ly" . "%s")\n'
                 % (ly.basename (),
                   name))

    snippet_map.write ('))\n')

def do_process_cmd (chunks, input_name):
    all_lys = filter (lambda x: is_derived_class (x.__class__,
                           Lilypond_snippet),
                      chunks)

    write_file_map (all_lys, input_name)
    ly_outdated = filter (lambda x: is_derived_class (x.__class__,
                                                      Lilypond_snippet)
                          and x.ly_is_outdated (), chunks)
    texstr_outdated = filter (lambda x: is_derived_class (x.__class__,
                                                          Lilypond_snippet)
                              and x.texstr_is_outdated (),
                              chunks)
    png_outdated = filter (lambda x: is_derived_class (x.__class__,
                                                        Lilypond_snippet)
                           and x.png_is_outdated (),
                           chunks)

    outdated = png_outdated + texstr_outdated + ly_outdated
    
    progress (_ ("Writing snippets..."))
    map (Lilypond_snippet.write_ly, ly_outdated)
    progress ('\n')

    if outdated:
        progress (_ ("Processing..."))
        progress ('\n')
        process_snippets (global_options.process_cmd, ly_outdated, texstr_outdated, png_outdated)
    else:
        progress (_ ("All snippets are up to date..."))
    progress ('\n')

def guess_format (input_filename):
    format = None
    e = os.path.splitext (input_filename)[1]
    if e in ext2format.keys ():
        # FIXME
        format = ext2format[e]
    else:
        error (_ ("cannot determine format for: %s" \
              % input_filename))
        exit (1)
    return format

def write_if_updated (file_name, lines):
    try:
        f = open (file_name)
        oldstr = f.read ()
        new_str = string.join (lines, '')
        if oldstr == new_str:
            progress (_ ("%s is up to date.") % file_name)
            progress ('\n')
            return
    except:
        pass

    progress (_ ("Writing `%s'...") % file_name)
    open (file_name, 'w').writelines (lines)
    progress ('\n')

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

def do_file (input_filename):
    # Ugh.
    if not input_filename or input_filename == '-':
        in_handle = sys.stdin
        input_fullname = '<stdin>'
    else:
        if os.path.exists (input_filename):
            input_fullname = input_filename
        elif global_options.format == LATEX and ly.search_exe_path ('kpsewhich'):
            input_fullname = os.popen ('kpsewhich ' + input_filename).read()[:-1]
        else:
            input_fullname = find_file (input_filename)

        note_input_file (input_fullname)
        in_handle = open (input_fullname)

    if input_filename == '-':
        input_base = 'stdin'
    else:
        input_base = os.path.basename \
                     (os.path.splitext (input_filename)[0])

    # Only default to stdout when filtering.
    if global_options.output_name == '-' or (not global_options.output_name and global_options.filter_cmd):
        output_filename = '-'
        output_file = sys.stdout
    else:
        # don't complain when global_options.output_name is existing
        output_filename = input_base + format2ext[global_options.format]
        if global_options.output_name:
            if not os.path.isdir (global_options.output_name):
                os.mkdir (global_options.output_name, 0777)
            os.chdir (global_options.output_name)
        else: 
            if (os.path.exists (input_filename) 
                and os.path.exists (output_filename) 
                and samefile (output_filename, input_fullname)):
             error (
             _ ("Output would overwrite input file; use --output."))
             exit (2)

    try:
        progress (_ ("Reading %s...") % input_fullname)
        source = in_handle.read ()
        progress ('\n')

        set_default_options (source)


        # FIXME: Containing blocks must be first, see
        #        find_toplevel_snippets.
        snippet_types = (
            'multiline_comment',
            'verbatim',
            'lilypond_block',
    #                'verb',
            'singleline_comment',
            'lilypond_file',
            'include',
            'lilypond',
        )
        progress (_ ("Dissecting..."))
        chunks = find_toplevel_snippets (source, snippet_types)

        if global_options.format == LATEX:
            for c in chunks:
                if (c.is_plain () and
                  re.search (r"\\begin *{document}", c.replacement_text())):
                    modify_preamble (c)
                    break
        progress ('\n')

        if global_options.filter_cmd:
            write_if_updated (output_filename,
                     [c.filter_text () for c in chunks])
        elif global_options.process_cmd:
            do_process_cmd (chunks, input_fullname)
            progress (_ ("Compiling %s...") % output_filename)
            progress ('\n')
            write_if_updated (output_filename,
                     [s.replacement_text ()
                     for s in chunks])
        
        def process_include (snippet):
            os.chdir (original_dir)
            name = snippet.substring ('filename')
            progress (_ ("Processing include: %s") % name)
            progress ('\n')
            return do_file (name)

        include_chunks = map (process_include,
                   filter (lambda x: is_derived_class (x.__class__,
                                     Include_snippet),
                       chunks))


        return chunks + reduce (lambda x,y: x + y, include_chunks, [])
        
    except Compile_error:
        os.chdir (original_dir)
        progress (_ ("Removing `%s'") % output_filename)
        progress ('\n')
        raise Compile_error

def do_options ():

    global global_options

    opt_parser = get_option_parser()
    (global_options, args) = opt_parser.parse_args ()

    if global_options.format in ('texi-html', 'texi'):
        global_options.format = TEXINFO
    global_options.use_hash = True

    global_options.include_path =  map (os.path.abspath, global_options.include_path)
    
    if global_options.warranty:
        warranty ()
        exit (0)
    if not args or len (args) > 1:
        opt_parser.print_help ()
        exit (2)
        
    return args

def psfonts_warning (options, basename):
    if options.format in (TEXINFO, LATEX):
        psfonts_file = os.path.join (options.output_name, basename + '.psfonts')
        output = os.path.join (options.output_name, basename +  '.dvi' )

        if not options.create_pdf:
            if not options.psfonts:
                warning (_ ("option --psfonts not used"))
                warning (_ ("processing with dvips will have no fonts"))
            else:
                progress ('\n')
                progress (_ ("DVIPS usage:"))
                progress ('\n')
                progress ("    dvips -h %(psfonts_file)s %(output)s" % vars ())
                progress ('\n')

def main ():
    # FIXME: 85 lines of `main' macramee??
    files = do_options ()

    file = files[0]

    basename = os.path.splitext (file)[0]
    basename = os.path.split (basename)[1]
    
    if not global_options.format:
        global_options.format = guess_format (files[0])

    formats = 'ps'
    if global_options.format in (TEXINFO, HTML, DOCBOOK):
        formats += ',png'

        
    if global_options.process_cmd == '':
        global_options.process_cmd = (lilypond_binary 
                                      + ' --formats=%s --backend eps ' % formats)

    if global_options.process_cmd:
        global_options.process_cmd += string.join ([(' -I %s' % ly.mkarg (p))
                              for p in global_options.include_path])

    if global_options.format in (TEXINFO, LATEX):
        ## prevent PDF from being switched on by default.
        global_options.process_cmd += ' --formats=eps '
        if global_options.create_pdf:
            global_options.process_cmd += "--pdf -dinclude-eps-fonts -dgs-load-fonts "
    
    if global_options.verbose:
        global_options.process_cmd += " --verbose "

    if global_options.padding_mm:
        global_options.process_cmd += " -deps-box-padding=%f " % global_options.padding_mm
        
    global_options.process_cmd += " -dread-file-list "

    identify ()

    try:
        chunks = do_file (file)
        if global_options.psfonts:
            fontextract.verbose = global_options.verbose
            snippet_chunks = filter (lambda x: is_derived_class (x.__class__,
                                       Lilypond_snippet),
                        chunks)

            psfonts_file = basename + '.psfonts' 
            if not global_options.verbose:
                progress (_ ("Writing fonts to %s...") % psfonts_file)
            fontextract.extract_fonts (psfonts_file,
                         [x.basename() + '.eps'
                          for x in snippet_chunks])
            if not global_options.verbose:
                progress ('\n')
            
    except Compile_error:
        exit (1)

    psfonts_warning (global_options, basename)

    inputs = note_input_file ('')
    inputs.pop ()

    base_file_name = os.path.splitext (os.path.basename (file))[0]
    dep_file = os.path.join (global_options.output_name, base_file_name + '.dep')
    final_output_file = os.path.join (global_options.output_name,
                     base_file_name
                     + '.%s' % global_options.format)
    
    os.chdir (original_dir)
    open (dep_file, 'w').write ('%s: %s' % (final_output_file, ' '.join (inputs)))

if __name__ == '__main__':
    main ()
