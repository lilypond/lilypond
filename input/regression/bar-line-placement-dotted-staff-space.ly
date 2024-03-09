\version "2.23.15"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "The dotted bar line never exceeds a common bar line.  This holds
for different staff space, entered with @code{\override StaffSymbol.staff-space}
as well."
}

\layout {
  \override StaffSymbol.staff-space = 1.23
  \autoLineBreaksOff
}

testBar = ";"
\include "bar-line-placement.ily"
