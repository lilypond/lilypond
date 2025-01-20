\version "2.25.24"

\header {
  texidoc="This test covers positioning arpeggio signs in the normal style.  On
each line, the first sign has its position set manually to the values in the
margin, and the second sign takes its position from the black notes.  The two
should match."
}

\layout {
  \context {
    \Score
    \arpeggioNormal
  }
}

testArpeggio = \arpeggio

\include "arpeggio-positions.ily"
