# -*- coding: utf-8 -*-

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
Docbook_snippet_res = {
    'lilypond':
         r'''(?smx)
          (?P<match>
          <(?P<inline>(inline)?)mediaobject>\s*
          <textobject.*?>\s*
          <programlisting\s+language="lilypond".*?(role="(?P<options>.*?)")?>
          (?P<code>.*?)
          </programlisting\s*>\s*
          </textobject\s*>\s*
          </(inline)?mediaobject>)''',

    'lilypond_block':
         r'''(?smx)
          (?P<match>
          <(?P<inline>(inline)?)mediaobject>\s*
          <textobject.*?>\s*
          <programlisting\s+language="lilypond".*?(role="(?P<options>.*?)")?>
          (?P<code>.*?)
          </programlisting\s*>\s*
          </textobject\s*>\s*
          </(inline)?mediaobject>)''',

    'lilypond_file':
         r'''(?smx)
          (?P<match>
          <(?P<inline>(inline)?)mediaobject>\s*
          <imageobject.*?>\s*
          <imagedata\s+
           fileref="(?P<filename>.*?\.ly)"\s*
           (role="(?P<options>.*?)")?\s*
           (/>|>\s*</imagedata>)\s*
          </imageobject>\s*
          </(inline)?mediaobject>)''',

    'multiline_comment':
         r'''(?smx)
          (?P<match>
          \s*(?!@c\s+)
          (?P<code><!--\s.*?!-->)
          \s)''',
}


Docbook_output = {
    FILTER: r'''<mediaobject>
  <textobject>
    <programlisting language="lilypond"
                    role="%(options)s">
%(code)s
    </programlisting>
  </textobject>
</mediaobject>''',

    OUTPUT: r'''<imageobject role="latex">
  <imagedata fileref="%(base)s.pdf" format="PDF"/>
</imageobject>
<imageobject role="html">
  <imagedata fileref="%(base)s.png" format="PNG"/>
</imageobject>''',

    PRINTFILENAME: r'''<textobject>
  <simpara>
    <ulink url="%(base)s%(ext)s">
      <filename>
        %(filename)s
      </filename>
    </ulink>
  </simpara>
</textobject>''',

    VERBATIM: r'''<programlisting>
%(verb)s</programlisting>''',

    VERSION: r'''%(program_version)s''',
}




class BookDocbookOutputFormat (BookBase.BookOutputFormat):
    def __init__ (self):
        BookBase.BookOutputFormat.__init__ (self)
        self.format = "docbook"
        self.default_extension = ".xml"
        self.snippet_res = Docbook_snippet_res
        self.output = Docbook_output
        self.handled_extensions = ['.lyxml']
        self.snippet_option_separator = '\s*'

    def adjust_snippet_command (self, cmd):
        if '--formats' not in cmd:
            return cmd + ' --formats=png,pdf '
        else:
            return cmd

    def snippet_output (self, basename, snippet):
        str = ''
        rep = snippet.get_replacements ();
        for image in snippet.get_images ():
            rep['image'] = image
            (rep['base'], rep['ext']) = os.path.splitext (image)
            str += self.output[OUTPUT] % rep
            str += self.output_print_filename (basename, snippet)
            if (snippet.substring('inline') == 'inline'):
                str = '<inlinemediaobject>' + str + '</inlinemediaobject>'
            else:
                str = '<mediaobject>' + str + '</mediaobject>'
        if VERBATIM in snippet.option_dict:
                rep['verb'] = BookBase.verbatim_html (snippet.verb_ly ())
                str = self.output[VERBATIM]  % rep + str
        return str


BookBase.register_format (BookDocbookOutputFormat ());
