\header {
  texidoc = "Staves can be started and stopped at command. "
}

\version "2.7.13"

\paper {
  raggedright = ##t
}

\relative c'' {
  b b \stopStaff b b \startStaff
  \clef bass
  c,, c
}
