\version "2.19.21"

\header {
  texidoc = "@code{ly:parser-include-string} should include the current
string like a file @code{\\include}."
}

#(ly:parser-include-string parser "\\relative { a4 b c d }")
