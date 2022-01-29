\version "2.23.7"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "The height of a short bar line is half the height of a
normal bar line, rounded up to an integer number of staff spaces.  It
is usually centered vertically, but on very short staves, it is
shifted down to distinguish it from a normal bar line."
}

testBar = ","
\include "bar-line-placement.ily"
