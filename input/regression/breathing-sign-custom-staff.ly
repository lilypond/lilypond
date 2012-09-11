\version "2.16.0"

\header {
  texidoc = "Breathing signs are positioned correctly on custom staves
which use @code{line-positions}."
}

\relative c' {
  \override Staff.StaffSymbol #'line-positions = #'(-7 -5 -3 -1)
  b2 \breathe b
  \override BreathingSign #'direction = #DOWN
  \breathe
}
