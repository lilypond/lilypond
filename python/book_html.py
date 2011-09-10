# -*- coding: utf-8 -*-

import book_base as BookBase
import copy
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
HTML_snippet_res = {
    'lilypond':
         r'''(?mx)
          (?P<match>
          <lilypond(\s+(?P<options>.*?))?\s*:\s*(?P<code>.*?)\s*/>)''',

    'lilypond_block':
         r'''(?msx)
          (?P<match>
          <lilypond\s*(?P<options>.*?)\s*>
          (?P<code>.*?)
          </lilypond\s*>)''',

    'lilypond_file':
         r'''(?mx)
          (?P<match>
          <lilypondfile\s*(?P<options>.*?)\s*>
          \s*(?P<filename>.*?)\s*
          </lilypondfile\s*>)''',

    'multiline_comment':
         r'''(?smx)(?P<match>\s*(?!@c\s+)(?P<code><!--\s.*?!-->)\s)''',

    'musicxml_file':
         r'''(?mx)
          (?P<match>
          <musicxmlfile\s*(?P<options>.*?)\s*>
          \s*(?P<filename>.*?)\s*
          </musicxmlfile\s*>)''',

    'verb':
         r'''(?x)(?P<match>(?P<code><pre>.*?</pre>))''',

    'verbatim':
         r'''(?xs)(?P<match>(?P<code><pre>\s.*?</pre>\s))''',

    'lilypondversion':
         r'''(?mx)(?P<match><lilypondversion\s*/>)''',
}


HTML_output = {
    FILTER: r'''<lilypond %(options)s>
%(code)s
</lilypond>
''',

    AFTER: r'''
 </a>
</p>''',

    BEFORE: r'''<p>
 <a href="%(base)s%(ext)s">''',

    OUTPUT: r'''
  <img align="middle"
       border="0"
       src="%(image)s"
       alt="%(alt)s">''',

    PRINTFILENAME: '<p><tt><a href="%(base)s%(ext)s">%(filename)s</a></tt></p>',

    QUOTE: r'''<blockquote>
%(str)s
</blockquote>
''',

    VERBATIM: r'''<pre>
%(verb)s</pre>''',

    VERSION: r'''%(program_version)s''',
}







class BookHTMLOutputFormat (BookBase.BookOutputFormat):
    def __init__ (self):
        BookBase.BookOutputFormat.__init__ (self)
        self.format = "html"
        self.default_extension = ".html"
        self.snippet_res = HTML_snippet_res
        self.output = HTML_output
        self.handled_extensions = ['.html', '.xml','.htmly']
        self.snippet_option_separator = '\s*'

    def split_snippet_options (self, option_string):
        if option_string:
            options = re.findall('[-\w\.-:]+(?:\s*=\s*(?:"[^"]*"|\'[^\']*\'|\S+))?',
                                 option_string)
            options = [re.sub('^([^=]+=\s*)(?P<q>["\'])(.*)(?P=q)', '\g<1>\g<3>', opt)
                       for opt in options]
            return options
        return []

    def adjust_snippet_command (self, cmd):
        if '--formats' not in cmd:
            return cmd + ' --formats=png '
        else:
            return cmd

    def snippet_output (self, basename, snippet):
        str = ''
        rep = snippet.get_replacements ();
        rep['base'] = basename
        rep['filename'] = os.path.basename (snippet.filename)
        rep['ext'] = snippet.ext
        str += self.output_print_filename (basename, snippet)
        if VERBATIM in snippet.option_dict:
            rep['verb'] = BookBase.verbatim_html (snippet.verb_ly ())
            str += self.output[VERBATIM] % rep
        if QUOTE in snippet.option_dict:
            str = self.output[QUOTE] % {'str': str}

        str += self.output[BEFORE] % rep
        for image in snippet.get_images ():
            rep1 = copy.copy (rep)
            rep1['image'] = image
            (rep1['base'], rep1['ext']) = os.path.splitext (image)
            rep1['alt'] = snippet.option_dict[ALT]
            str += self.output[OUTPUT] % rep1

        str += self.output[AFTER] % rep
        return str

    def required_files (self, snippet, base, full, required_files):
        return self.required_files_png (snippet, base, full, required_files)


BookBase.register_format (BookHTMLOutputFormat ());
