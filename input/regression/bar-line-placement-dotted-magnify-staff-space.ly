\version "2.23.15"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "The dotted bar line never exceeds a common bar line.  This holds
for different staff space, caused by  @code{\magnifyStaff} as well."
}

\layout {
  \autoLineBreaksOff
  \context {
    \Staff
    \magnifyStaff #5/4
  }
}

testBar = ";"
\include "bar-line-placement.ily"
