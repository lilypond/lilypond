\version "2.19.36"

\header {
  texidoc = "The ledger-extra grob property increases
the number of ledger lines drawn, but they are not
drawn on top of staff lines."
}

notes = \relative {
  \time 6/8
  a4. b | c d | e f |
  g4. a | b c | d e |
  f4. g | a b |
}

\new Staff {
  \notes
}

\new Staff {
  \override Staff.StaffSymbol.ledger-extra = 1
  \notes
}

\new Staff {
  \override Staff.StaffSymbol.ledger-extra = 2
  \notes
}
