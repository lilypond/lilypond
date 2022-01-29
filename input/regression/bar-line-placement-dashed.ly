\version "2.23.7"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "A dashed bar line extends approximately as far as a
normal bar line.  The center-to-center distance between dashes is
uniformly one staff space.  At the vertical center of the staff is
either a dash or the midpoint between dashes."
}

testBar = "!"
\include "bar-line-placement.ily"
