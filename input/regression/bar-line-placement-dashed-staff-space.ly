\version "2.23.15"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "The dashed bar line scales with different staff space, entered with
@code{\override StaffSymbol.staff-space}."
}

\layout {
  \override StaffSymbol.staff-space = 1.23
  \autoLineBreaksOff
}

testBar = "!"
\include "bar-line-placement.ily"
