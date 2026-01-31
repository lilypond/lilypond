\version "2.25.24"

\header {
  texidoc="This test covers positioning arpeggio signs in the dashed-parenthesis
style.  On each line, the first sign has its position set manually to the values
in the margin, and the second sign takes its position from the black notes.  The
two should match."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    \arpeggioParenthesisDashed
  }
}

testCommand = \arpeggio
testGrobName = #'Arpeggio

\include "arpeggio-positions.ily"
