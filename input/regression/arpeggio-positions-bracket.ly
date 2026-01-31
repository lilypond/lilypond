\version "2.25.24"

\header {
  texidoc="This test covers positioning non-arpeggio brackets.  On each line,
the first bracket has its position set manually to the values in the margin, and
the second bracket takes its position from the black notes.  The two should
match."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    \arpeggioBracket
  }
}

testCommand = \arpeggio
testGrobName = #'Arpeggio

\include "arpeggio-positions.ily"
