\header {
  texidoc = "String numbers can be added to chords. They use the same
positioning mechanism as finger instructions."

}

\version "2.5.17"
\paper {
  raggedright = ##t
}

\relative {
  <c\1 e\2 g\3>
  \set fingeringOrientations = #'(down right up)
  <c\1 e\2 g\3>
}
