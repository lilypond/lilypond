\version "2.19.21"

\header {
  texidoc = "Arpeggios stil work in the absence of a staff-symbol."
}

\new Staff \with { \remove "Staff_symbol_engraver" }
\relative {
  <c' c'>\arpeggio
}
