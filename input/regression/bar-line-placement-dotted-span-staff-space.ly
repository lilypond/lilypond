\version "2.23.15"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "The center-to-center distance between the dots in a
dotted span bar line is approximately one staff space, taken from layout (not
from the staff symbol).  The dots of the span bar do not collide with staff
lines or with the dots of in-staff bar lines.  They never exceed the extent of
the span bar (made visible by the thick span bar line).  This holds for
different staff space, caused by @code{\override StaffSymbol.staff-space} as
well."
}


\layout {
  \context {
    \Staff
    \override StaffSymbol.staff-space = 1.5
  }
}

testBar = ";"
\include "bar-line-placement-span.ily"
