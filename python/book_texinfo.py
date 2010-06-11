# -*- coding: utf-8 -*-

import re
import book_base as BookBase
from book_snippets import *

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
@image{%(info_image_path)s,,,%(alt)s,%(ext)s}
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

    NOQUOTE: r'''@format
%(str)s@end format
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

    def get_line_width (self, source):
        for regex in texinfo_line_widths:
            # FIXME: @layout is usually not in
            # chunk #0:
            #
            #  \input texinfo @c -*-texinfo-*-
            #
            # Bluntly search first K items of
            # source.
            # s = chunks[0].replacement_text ()
            if re.search (regex, source[:1024]):
                return texinfo_line_widths[regex]
        return None

    def adjust_snippet_command (self, cmd):
        if '--formats' not in cmd:
            return cmd + ' --formats=png '
        else:
            return cmd

    def output_info (self, basename, snippet):
        str = ''
        rep = snippet.get_replacements ();
        for image in snippet.get_images ():
            rep1 = copy.copy (rep)
            (rep1['base'], rep1['ext']) = os.path.splitext (image)
            rep1['image'] = image

            # URG, makeinfo implicitly prepends dot to extension.
            # Specifying no extension is most robust.
            rep1['ext'] = ''
            rep1['alt'] = snippet.option_dict[ALT]
            rep1['info_image_path'] = os.path.join (self.global_options.info_images_dir, rep1['base'])
            str += self.output[OUTPUTIMAGE] % rep1

        rep['base'] = basename
        str += self.output[OUTPUT] % rep
        return str

    def snippet_output (self, basename, snippet):
        str = self.output_print_filename (basename, snippet)
        base = basename
        if DOCTITLE in snippet.option_dict:
            doctitle = base + '.doctitle'
            translated_doctitle = doctitle + self.document_language
            if os.path.exists (translated_doctitle):
                str += '@lydoctitle %s\n\n' % open (translated_doctitle).read ()
            elif os.path.exists (doctitle):
                str += '@lydoctitle %s\n\n' % open (doctitle).read ()
        if TEXIDOC in snippet.option_dict:
            texidoc = base + '.texidoc'
            translated_texidoc = texidoc + self.document_language
            if os.path.exists (translated_texidoc):
                str += '@include %(translated_texidoc)s\n\n' % vars ()
            elif os.path.exists (texidoc):
                str += '@include %(texidoc)s\n\n' % vars ()

        substr = ''
        rep = snippet.get_replacements ();
        if VERBATIM in snippet.option_dict:
            rep['version'] = ''
            if ADDVERSION in snippet.option_dict:
                rep['version'] = self.output[ADDVERSION]
            rep['verb'] = snippet.verb_ly ()
            substr = self.output[VERBATIM] % rep
        substr += self.output_info (basename, snippet)
        if LILYQUOTE in snippet.option_dict:
            substr = self.output[QUOTE] % {'str': substr}
        str += substr

#                str += ('@ifinfo\n' + self.output_info () + '\n@end ifinfo\n')
#                str += ('@tex\n' + self.output_latex () + '\n@end tex\n')
#                str += ('@html\n' + self.output_html () + '\n@end html\n')

        if QUOTE in snippet.option_dict:
            str = self.output[QUOTE] % {'str': str}

        # need par after image
        str += '\n'

        return str

    def required_files (self, snippet, base, full, required_files):
        return self.required_files_png (snippet, base, full, required_files)



BookBase.register_format (BookTexinfoOutputFormat ());
