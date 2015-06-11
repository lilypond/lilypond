\version "2.19.22"

\header {
  texidoc = "@code{ly:parser-include-string} should include the current
string like a file @code{\\include}."
}

#(ly:parser-include-string "\\relative { a4 b c d }")
