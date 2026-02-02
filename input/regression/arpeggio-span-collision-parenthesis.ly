\version "2.25.34"

\header {
  texidoc = "Cross-staff or -voice vertical slurs which include single note
heads as anchors do not collide with previous note heads or prefatory material."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    \arpeggioParenthesis
  }
}

testCommand = \arpeggio
testConnectPropertyName = #'connectArpeggios

\include "arpeggio-span-collision.ily"
