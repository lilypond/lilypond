\version "2.25.27"

\header {
  texidoc = "If not specified otherwise, both ledger lines and note heads
are positioned on integer vertical positions, regardless of the staff line
positions given in the @code{line-positions} property."
}

{
  \override Staff.StaffSymbol.line-positions = #'(-2.6 -2)
  d' e' f' g'
}
