
\version "2.11.51"

\header{
  texidoc=" The number of stafflines of a staff can be set.  Ledger
lines both on note heads and rests, as well as barlines, are
adjusted accordingly.  "
}


\new Staff \with {
  \override StaffSymbol #'line-count = #3
}
\relative c {
  c' c c c | g' g g g 	\bar ":|"
}

