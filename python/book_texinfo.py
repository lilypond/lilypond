# -*- coding: utf-8 -*-

import re
import tempfile
import subprocess
import sys
import book_base as BookBase
from book_snippets import *
import lilylib as ly
global _;_=ly._


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
TexInfo_snippet_res = {
    'include': r'''(?mx)
          ^(?P<match>
          @include\s+
           (?P<filename>\S+))''',

    'lilypond': r'''(?smx)
          ^[^\n]*?(?!@c\s+)[^\n]*?
          (?P<match>
          @lilypond\s*(
          \[
           \s*(?P<options>.*?)\s*
          \])?\s*{
           (?P<code>.*?)
          })''',

    'lilypond_block': r'''(?msx)
          ^(?P<match>
          @lilypond\s*(
          \[
           \s*(?P<options>.*?)\s*
          \])?\s+?
          ^(?P<code>.*?)
          ^@end\s+lilypond)\s''',

    'lilypond_file': r'''(?mx)
          ^(?P<match>
          @lilypondfile\s*(
          \[
           \s*(?P<options>.*?)\s*
          \])?\s*{
           (?P<filename>\S+)
          })''',

    'multiline_comment': r'''(?smx)
          ^(?P<match>
           (?P<code>
           @ignore\s
            .*?
           @end\s+ignore))\s''',

    'musicxml_file': r'''(?mx)
          ^(?P<match>
          @musicxmlfile\s*(
          \[
           \s*(?P<options>.*?)\s*
          \])?\s*{
           (?P<filename>\S+)
          })''',

    'singleline_comment': r'''(?mx)
          ^.*
          (?P<match>
           (?P<code>
           @c([ \t][^\n]*|)\n))''',

    # Don't do this: It interferes with @code{@{}.
    #        'verb': r'''(?P<code>@code{.*?})''',

    'verbatim': r'''(?sx)
          (?P<match>
           (?P<code>
           @example
            \s.*?
           @end\s+example\s))''',

    'lilypondversion': r'''(?mx)
         [^@](?P<match>
          @lilypondversion)[^a-zA-Z]''',

}


TexInfo_output = {
    ADDVERSION: r'''@example
\version @w{"@version{}"}
@end example
''',

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
@image{%(info_image_path)s,,,%(alt)s,}
@end ifinfo
@html
<p>
 <a href="%(base)s%(ext)s">
  <img align="middle"
       border="0"
       src="%(image)s"
       alt="%(alt)s">
 </a>
</p>
@end html
''',

    PRINTFILENAME: '''
@html
<a href="%(base)s%(ext)s">
@end html
@file{%(filename)s}
@html
</a>
@end html
    ''',

    QUOTE: r'''@quotation
%(str)s@end quotation
''',

    VERBATIM: r'''@exampleindent 0
%(version)s@verbatim
%(verb)s@end verbatim
''',

    VERSION: r'''%(program_version)s''',
}


texinfo_line_widths = {
    '@afourpaper': '160\\mm',
    '@afourwide': '6.5\\in',
    '@afourlatex': '150\\mm',
    '@smallbook': '5\\in',
    '@letterpaper': '6\\in',
}


###
# Retrieve dimensions from texinfo
TEXINFO_INSPECTION_DOCUMENT = r'''
\input texinfo
@setfilename Texinfo_width_test
@settitle Texinfo width test
%(preamble)s

@message{Global: textwidth=@the@hsize,exampleindent=@the@lispnarrowing}

@bye
'''

def get_texinfo_width_indent (source, global_options):
    #TODO: Check for end of header command "@c %**end of header"
    #      only use material before that comment ?

    # extract all relevant papter settings from the input:
    pagesize = None
    texinfo_paper_size_regexp = r'''(@(?:afourpaper|afourwide|afourlatex|afivepaper|smallbook|letterpaper))''';
    m = re.search (texinfo_paper_size_regexp, source);
    if m:
        pagesize = m.group (1)

    relevant_settings_regexp = r'''(@(?:fonttextsize|pagesizes|cropmarks|exampleindent).*)\n''';
    m = re.findall (relevant_settings_regexp, source);
    if pagesize:
        m.insert (0, pagesize);
    # all relevant options to insert into the test document:
    preamble = "\n".join (m);

    texinfo_document = TEXINFO_INSPECTION_DOCUMENT % {'preamble': preamble}

    (handle, tmpfile) = tempfile.mkstemp('.texi')
    outfile = os.path.splitext (tmpfile)[0] + '.pdf'

    tmp_handle = os.fdopen (handle,'w')
    tmp_handle.write (texinfo_document)
    tmp_handle.close ()

    # Work around a texi2pdf bug: if LANG=C is not given, a broken regexp is
    # used to detect relative/absolute paths, so the absolute path is not
    # detected as such and this command fails:
    progress (_ ("Running texi2pdf on file %s to detect default page settings.\n") % tmpfile);

    # execute the command and pipe stdout to the parameter_string:
    cmd = '%s -c -o %s %s' % (global_options.texinfo_program, outfile, tmpfile);
    ly.debug_output ("Executing: %s\n" % cmd);
    run_env = os.environ.copy()
    run_env['LC_ALL'] = 'C'

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
        returncode = os.system(cmd)
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
    if os.path.exists(outfile):
        os.unlink (outfile)

    # Find textwidth and exampleindent and format it as \\mm or \\in
    # Use defaults if they cannot be extracted
    textwidth = 0
    m = re.search ('textwidth=([0-9.]+)pt', parameter_string)
    if m:
        val = float (m.group (1))/72.27
        if pagesize and pagesize.startswith ("@afour"):
            textwidth = "%g\\mm" % round (val*25.4, 3);
        else:
            textwidth = "%g\\in" % round (val, 3);
    else:
        textwidth = texinfo_line_widths.get(pagesize, "6\\in")

    exampleindent = 0
    m = re.search ('exampleindent=([0-9.]+)pt', parameter_string)
    if m:
        val = float (m.group (1))/72.27
        if pagesize and pagesize.startswith ("@afour"):
            exampleindent = "%g\\mm" % round (val*25.4, 3);
        else:
            exampleindent = "%g\\in" % round (val, 3);
    else:
        exampleindent = "0.4\\in"

    retval = {LINE_WIDTH: textwidth, EXAMPLEINDENT: exampleindent}
    ly.debug_output ("Auto-detected values are: %s\n" % retval);
    return retval;



texinfo_lang_re = re.compile ('(?m)^@documentlanguage (.*?)( |$)')

class BookTexinfoOutputFormat (BookBase.BookOutputFormat):
    def __init__ (self):
        BookBase.BookOutputFormat.__init__ (self)
        self.format = "texinfo"
        self.default_extension = ".texi"
        self.snippet_res = TexInfo_snippet_res
        self.output = TexInfo_output
        self.handled_extensions = ['.itely', '.tely', '.texi', '.texinfo']
        self.snippet_option_separator = '\s*,\s*'

    def can_handle_format (self, format):
        return (BookBase.BookOutputFormat.can_handle_format (self, format) or
               (format in ['texi-html', 'texi']))

    def process_options (self, global_options):
        self.process_options_pdfnotdefault (global_options)

    def get_document_language (self, source):
        m = texinfo_lang_re.search (source)
        if m and not m.group (1).startswith ('en'):
            return m.group (1)
        else:
            return ''

    def init_default_snippet_options (self, source):
        texinfo_defaults = get_texinfo_width_indent (source, self.global_options);
        self.default_snippet_options.update (texinfo_defaults)
        BookBase.BookOutputFormat.init_default_snippet_options (self, source)

    def adjust_snippet_command (self, cmd):
        if '--formats' not in cmd:
            return cmd + ' --formats=png '
        else:
            return cmd

    def output_info (self, basename, snippet):
        str = ''
        rep = snippet.get_replacements ();
        rep['base'] = basename
        rep['filename'] = os.path.basename (snippet.filename)
        rep['ext'] = snippet.ext
        for image in snippet.get_images ():
            rep1 = copy.copy (rep)
            rep1['base'] = os.path.splitext (image)[0]
            rep1['image'] = image
            rep1['alt'] = snippet.option_dict[ALT]
            rep1['info_image_path'] = os.path.join (self.global_options.info_images_dir, rep1['base'])
            str += self.output[OUTPUTIMAGE] % rep1

        str += self.output[OUTPUT] % rep
        return str

    def snippet_output (self, basename, snippet):
        str = ''
        base = basename
        if DOCTITLE in snippet.option_dict:
            doctitle = base + '.doctitle'
            translated_doctitle = doctitle + self.document_language
            if os.path.exists (translated_doctitle):
                str += '\n@lydoctitle %s\n\n' % open (translated_doctitle).read ()
            elif os.path.exists (doctitle):
                str += '\n@lydoctitle %s\n\n' % open (doctitle).read ()
        if TEXIDOC in snippet.option_dict:
            texidoc = base + '.texidoc'
            translated_texidoc = texidoc + self.document_language
            if os.path.exists (translated_texidoc):
                str += '@include %(translated_texidoc)s\n\n' % vars ()
            elif os.path.exists (texidoc):
                str += '@include %(texidoc)s\n\n' % vars ()
        str += self.output_print_filename (basename, snippet)

        substr = ''
        rep = snippet.get_replacements ();
        if VERBATIM in snippet.option_dict:
            rep['version'] = ''
            if ADDVERSION in snippet.option_dict:
                rep['version'] = self.output[ADDVERSION]
            rep['verb'] = snippet.verb_ly ()
            substr = self.output[VERBATIM] % rep
        substr += self.output_info (basename, snippet)
        if QUOTE in snippet.option_dict:
            substr = self.output[QUOTE] % {'str': substr}
        str += substr

        # need par after image
        str += '\n'

        return str

    def required_files (self, snippet, base, full, required_files):
        return self.required_files_png (snippet, base, full, required_files)



BookBase.register_format (BookTexinfoOutputFormat ());
