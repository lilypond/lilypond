\version "2.25.24"

\header {
  texidoc="This test covers positioning down-arpeggio signs.  On each line, the
first sign has its position set manually to the values in the margin, and the
second sign takes its position from the black notes.  The two should match."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    \arpeggioArrowDown
  }
}

testArpeggio = \arpeggio % \downArpeggio would make more sense

\include "arpeggio-positions.ily"
