\version "2.23.7"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "The center-to-center distance between the dots in a
dotted bar line is uniformly one staff space.  At the vertical center
of the staff is either a dot or the midpoint between dots, whichever
places fewer dots on staff lines."
}

testBar = ";"
\include "bar-line-placement.ily"
