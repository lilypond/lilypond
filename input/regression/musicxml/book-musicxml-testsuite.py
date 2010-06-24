# -*- coding: utf-8 -*-

import book_base as BookBase
import book_texinfo as BookTexinfo
import book_snippets as BookSnippet
import lilylib as ly

MusicXMLOutputImage = r'''@noindent
@ifinfo
@image{%(info_image_path)s,,,%(alt)s,%(ext)s}
@end ifinfo
@html
<p>
 <a href="%(base)s%(ext)s">
  <img align="middle" border="0" src="%(image)s" alt="%(alt)s">
 </a>
</p>
@end html
'''

MusicXMLOutput = r'''
@iftex
@include %(base)s-systems.texi
@end iftex
'''

MusicXMLPrintFilename = r'''
@html
<a href="%(base)s%(ext)s">
@end html
@file{%(filename)s}
@html
</a>
@end html
'''




class BookMusicXML (BookTexinfo.BookTexinfoOutputFormat):
    def __init__ (self):
        BookTexinfo.BookTexinfoOutputFormat.__init__ (self)
        self.format = "MusicXMLTest"
        self.output[BookBase.OUTPUTIMAGE] = MusicXMLOutputImage
        self.output[BookBase.OUTPUT] = MusicXMLOutput
        self.output[BookBase.PRINTFILENAME] = MusicXMLPrintFilename
    def snippet_class (self, type):
      if type == "musicxml_file":
          return MusicXMLTestSuiteSnippet
      else:
          return BookSnippet.snippet_type_to_class.get (type, BookSnippet.Snippet)
    def snippet_output (self, basename, snippet):
        return BookTexinfo.BookTexinfoOutputFormat.snippet_output (self, basename, snippet)


class MusicXMLTestSuiteSnippet (BookSnippet.MusicXMLFileSnippet):
    def __init__ (self, type, match, formatter, line_number, global_options):
        BookSnippet.MusicXMLFileSnippet.__init__ (self, type, match, formatter, line_number, global_options)

## TODO: Customize output with renderings from other MusicXML-supporting
#        applications. Also add some link to the intermediate .ly file


BookBase.register_format (BookMusicXML ());
