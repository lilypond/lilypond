\version "2.25.34"

\header {
  texidoc="This test covers positioning non-arpeggiato brackets.  On each line,
the first bracket has its position set manually to the values in the margin, and
the second bracket takes its position from the black notes.  The two should
match."
}

#(ly:set-option 'warning-as-error #t)

testCommand = \nonArpeggiato
testGrobName = #'ChordBracket

\include "arpeggio-positions.ily"
