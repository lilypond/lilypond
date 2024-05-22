\version "2.25.17"

\header {
  texidoc = "@code{\\include} accepts a string-valued variable.  The expected
output is three quarter notes."
}

whatToInclude = "include-identifier.ily"
{ c'4 \include \whatToInclude c'4 }
