\version "2.25.34"

\header {
  texidoc="Setting @code{ChordBracket@/.direction} to @code{#RIGHT} makes the
edges of the bracket point toward the left.  The first quarter-note chord has
the bracket on the right; the second has it on the left.  Ditto for the
half-note chords."
}

#(ly:set-option 'warning-as-error #t)

testCommand = \nonArpeggiato
testGrobName = #'ChordBracket

\include "arpeggio-at-right.ily"
