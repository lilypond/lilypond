\version "2.25.15"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "This test shows the placement of dashed bar lines with
span bars in various staff configurations and changed staff space via
@code{\override StaffSymbol.staff-space}."
}

\layout {
  \context {
    \Staff
    \override StaffSymbol.staff-space = 1.5
  }
}

testBar = "!"
\include "bar-line-placement-span.ily"
