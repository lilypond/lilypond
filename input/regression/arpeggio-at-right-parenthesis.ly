\version "2.25.24"

\header {
  texidoc="Setting @code{Arpeggio.direction} to @code{#RIGHT} makes a
parenthesis point toward the left.  The first quarter-note chord has the bracket
on the right; the second has it on the left.  Ditto for the half-note chords."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    \arpeggioParenthesis
  }
}

testCommand = \arpeggio
testGrobName = #'Arpeggio

\include "arpeggio-at-right.ily"
