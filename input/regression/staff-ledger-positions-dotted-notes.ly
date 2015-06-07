\header {

  texidoc = "When the vertical positions of ledger lines have been
customized by setting the @code{ledger-positions} property of the
StaffSymbol, and a dotted note falls on a ledger line, the dot is shifted
up to avoid the ledger line (just as with uncustomized ledger lines)."
}

\version "2.19.21"

\new Staff \relative {
  \override Staff.StaffSymbol.line-positions = #'(-3 0 3)
  \override Staff.StaffSymbol.ledger-positions = #'(-3 0)
  \override Staff.StaffSymbol.ledger-extra = #1
  f4. g a b c d g' a b c d e
}
