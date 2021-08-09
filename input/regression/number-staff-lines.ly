
\version "2.19.21"

\header{
  texidoc=" The number of stafflines of a staff can be set.  Ledger
lines both on note heads and rests, as well as bar lines, are
adjusted accordingly."
}


\new Staff \with {
  \override StaffSymbol.line-count = #3
}
\relative {
  c' c c c | g' g g g 	\bar ":|."
}

