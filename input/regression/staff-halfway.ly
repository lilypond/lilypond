\header {
  texidoc = "Staves can be started and stopped at command. "
}

\version "2.19.21"

\paper {
  ragged-right = ##t
}

\relative {
  b' b \stopStaff b b \startStaff
  \clef bass
  c,, c
}
