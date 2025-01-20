\version "2.25.24"

\header {
  texidoc="This test covers positioning arpeggio signs in the arrow-up style.
On each line, the first sign has its position set manually to the values in the
margin, and the second sign takes its position from the black notes.  The two
should match."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    \arpeggioArrowUp
  }
}

testArpeggio = \arpeggio

\include "arpeggio-positions.ily"
