\version "2.25.34"

\header {
  texidoc = "Cross-staff or -voice chord slurs which include single note heads
as anchors do not collide with previous note heads or prefatory material."
}

testCommand = \chordSlur
testConnectPropertyName = #'connectChordSlurs

\include "arpeggio-span-collision.ily"
