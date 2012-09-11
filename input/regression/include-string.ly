\version "2.16.0"

\header {
  texidoc = "@code{ly:parser-include-string} should include the current
string like a file @code{\\include}."
}

#(ly:parser-include-string parser "\\relative c' { a4 b c d }")
