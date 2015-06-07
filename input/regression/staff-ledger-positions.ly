\header {

  texidoc = "The vertical positions of ledger lines may be customised
by setting the @code{ledger-positions} property of the StaffSymbol.
The given pattern is repeated.  Bracketed groups are always shown together:
either all or none are shown.  Ledger lines can be set to appear sooner or
later by setting the @code{ledger-extra} property."
}

\version "2.19.21"

\new Staff \relative {
  \override Staff.StaffSymbol.line-positions = #'(-5 -2 -1 2 5 6)
  \override Staff.StaffSymbol.ledger-positions = #'(-5 (-2 -1) 2)
  \override Staff.StaffSymbol.ledger-extra = #1
  g,4 c e b' c'' e g
}
