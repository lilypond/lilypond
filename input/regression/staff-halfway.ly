\header {
  texidoc = "Staves can be started and stopped at command. "
}

\version "2.6.0"

\paper {
  raggedright = ##t
}

\relative c'' {
  b b \stopStaff b b \startStaff
  \clef bass
  c,, c
}
