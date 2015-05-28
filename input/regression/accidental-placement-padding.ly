\version "2.19.21"

\header {
  texidoc = "Accidental padding works for all accidentals, including
those modifying the same pitch."
}

\relative {
  \override Staff.AccidentalPlacement.padding = #2
  <ces' cis>1
  <ces des>1
}
