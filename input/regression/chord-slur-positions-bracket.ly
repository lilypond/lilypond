\version "2.25.34"

\header {
  texidoc="This test covers positioning vertical slurs.  On each line, the first
slur has its position set manually to the values in the margin, and the second
slur takes its position from the black notes.  The two should match."
}

#(ly:set-option 'warning-as-error #t)

testCommand = \chordSlur
testGrobName = #'ChordSlur

\include "arpeggio-positions.ily"
