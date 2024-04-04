\version "2.23.15"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "The dashed bar line scales with different staff space, entered with
@code{\magnifyStaff}."
}

\layout {
  \autoLineBreaksOff
  \context {
    \Staff
    \magnifyStaff #5/4
  }
}

testBar = "!"
\include "bar-line-placement.ily"
