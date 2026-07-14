\version "2.27.2"

\header {
  texidoc = "There is an implied bar check at the end of
@code{\\measure}.  The visual output of this test is not important."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "bar check failed at: %s") 1/2)

\fixed c' {
  \measure { c4 d \set Timing.measureLength = 1 e f g2 }
}
