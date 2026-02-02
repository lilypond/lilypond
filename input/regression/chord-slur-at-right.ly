\version "2.25.34"

\header {
  texidoc="Setting @code{ChordSlur@/.direction} to @code{#RIGHT} makes the ends
of the slur point toward the left.  The first quarter-note chord has the slur
on the right; the second has it on the left.  Ditto for the half-note chords."
}

#(ly:set-option 'warning-as-error #t)

testCommand = \chordSlur
testGrobName = #'ChordSlur

\include "arpeggio-at-right.ily"
