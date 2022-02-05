\header {
  
  texidoc = "In some fonts, the same glyph is used to render differing
code points.  In this file, the Japanese font uses the same glyph for
representing U+898B and U+2F92. However, when running the output of
this file through @command{pdftotext}, the original codepoints are
returned."
  % suppress distracting tagline text
  tagline = ""
}

\version "2.23.6"
\markup {
  \override #'(font-name . "Noto Serif JP")
  { #(ly:wide-char->utf-8 #x898b)
    #(ly:wide-char->utf-8 #x2f92)
  }
}
  

