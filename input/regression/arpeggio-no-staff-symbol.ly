\version "2.13.17"

\header {
  texidoc = "Arpeggios stil work in the absence of a staff-symbol."
}

\new Staff \with { \remove "Staff_symbol_engraver" }
\relative c' {
  <c c'>\arpeggio
}
