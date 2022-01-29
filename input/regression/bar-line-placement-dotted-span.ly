\version "2.23.7"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "The center-to-center distance between the dots in a
dotted span bar line is uniformly one staff space.  The dots of the
span bar do not collide with staff lines or with the dots of in-staff
bar lines."
}

testBar = ";"
\include "bar-line-placement-span.ily"
