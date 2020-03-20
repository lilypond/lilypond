import unittest

import book_base
import book_snippets
import book_texinfo

class Dummy:
    pass

class TestFindSnippets(unittest.TestCase):
    def test_basic_texi(self):
        formatter = book_texinfo.BookTexinfoOutputFormat()
        global_options = Dummy()
        global_options.information = {"program_version": "1.2.3"}
        chunks = book_base.find_toplevel_snippets(r"""\input texinfo @c -*- coding: utf-8; mode: texinfo; -*- 1
@setfilename texinfo-include-file.info 2
@settitle Include lilypond files in texinfo 3
4
@node Top 5
@top Include lilypond files in texinfo 6

Lilypond files included in texinfo without any options: 8

@lilypondfile{input/regression/les-nereides.ly}

From a subdirectory: 12

@lilypondfile{input/regression/morgenlied.ly}

Within a lilypond block: 16

@lilypond
 % \include "include/myvar.ily"
  \relative c'' { c e g }
@end lilypond

Include a file that includes a file:
""",  formatter, global_options)

        # comment
        types  = [book_snippets.Substring,
                  book_snippets.Snippet, # @c
                  book_snippets.Substring,
                  book_snippets.LilypondFileSnippet,
                  book_snippets.Substring,
                  book_snippets.LilypondFileSnippet,
                  book_snippets.Substring,
                  book_snippets.LilypondSnippet,
                  book_snippets.Substring]
        for i in range(0, len(types)):
            self.assertIsInstance(chunks[i], types[i])
        self.assertEqual(chunks[7].line_number, 18)

if __name__ == '__main__':
    unittest.main()
