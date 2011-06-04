\version "2.14.0"

\header {
  texidoc = "Arpeggios stil work in the absence of a staff-symbol."
}

\new Staff \with { \remove "Staff_symbol_engraver" }
\relative c' {
  <c c'>\arpeggio
}
