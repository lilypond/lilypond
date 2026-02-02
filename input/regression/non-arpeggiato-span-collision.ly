\version "2.25.34"

\header {
  texidoc = "Cross-staff or -voice non-arpeggiato brackets which include single
note heads as anchors do not collide with previous note heads or prefatory
material."
}

#(ly:set-option 'warning-as-error #t)

testCommand = \nonArpeggiato
testConnectPropertyName = #'connectChordBrackets

\include "arpeggio-span-collision.ily"
