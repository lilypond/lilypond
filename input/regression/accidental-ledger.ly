\version "2.19.21"

\header {
  texidoc = "Ledger lines are shortened when there are accidentals.
Depending on the accidental, more than a single staff line gets shortened.
This happens only if the accidental is horizontally close to the head."
}

\relative {
  \override Staff.StaffSymbol.ledger-positions = #'(0 1)
  \override Staff.StaffSymbol.ledger-extra = 4

  disis4 dis d des |
  deses4 eses es e |
  eis4 eisis eses''' es |
  e4 eis eisis feses |
  fes4 f fis fisis |
  <cis,, gis'>4
}
