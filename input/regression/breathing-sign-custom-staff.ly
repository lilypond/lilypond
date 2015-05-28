\version "2.19.21"

\header {
  texidoc = "Breathing signs are positioned correctly on custom staves
which use @code{line-positions}."
}

\relative {
  \override Staff.StaffSymbol.line-positions = #'(-7 -5 -3 -1)
  b2 \breathe b
  \override BreathingSign.direction = #DOWN
  \breathe
}
