\version "2.23.7"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "A tick bar line is a short line the length of a staff
space.  It is usually centered on the topmost bar line, but if there
are fewer than two bar lines, it floats at the height of a normal bar
line."
}

testBar = "'"
\include "bar-line-placement.ily"
