\header {
  texidoc = "String numbers can be added to chords. They use the same
positioning mechanism as finger instructions."

}

\version "2.19.21"
\paper {
  ragged-right = ##t
}

\relative {
  <c'\1 e\2 g\3>
  c4
  \set fingeringOrientations = #'(down left up)
  \set stringNumberOrientations = #'(down right up)
  <c\1 e\2 g\3>
  <c-1\1 e-2\2 g-3\3>
}
