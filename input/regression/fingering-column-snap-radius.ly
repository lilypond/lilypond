\version "2.17.15"

\header {
  texidoc = "Horizontally-offset @code{Fingerings} align along the Y axis when
they are within @code{FingeringColumn.snap-radius} of each other.
"
}

\relative f'' {
  \set fingeringOrientations = #'(left)
  <cis-1 a-1 e-1>4
  <ceses-1 a-1 e-1>4
}
