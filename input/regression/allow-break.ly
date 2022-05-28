\version "2.23.10"

\header {
  texidoc = "The @code{\\allowBreak} command inserts a break point
regardless of bar lines, unbreakable spanners, etc.  This test
should have a break in the middle of a measure."
}

\repeat unfold 10 {
  c'4 \allowBreak
  4 \allowBreak
  4 \allowBreak
  4 \noBreak
}
