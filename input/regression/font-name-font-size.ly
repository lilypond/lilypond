\version "2.13.14"

\header {
  texidoc = "
Setting the @code{font-name} property does not change the font
size.  The two strings below should be concatenated and have the
same font size.
"
}

\markup \concat {
  "string"
  \override #'(font-name . "New Century Schoolbook")
  "string"
}
