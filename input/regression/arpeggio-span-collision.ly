\version "2.25.33"

\header {
  texidoc = "Cross-staff or -voice arpeggios which include single
note heads as anchors do not collide with previous note heads or
prefatory material."
}

#(ly:set-option 'warning-as-error #t)

testCommand = \arpeggio
testConnectPropertyName = #'connectArpeggios

\include "arpeggio-span-collision.ily"
