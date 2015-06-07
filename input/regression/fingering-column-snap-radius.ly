\version "2.19.21"

\header {
  texidoc = "Horizontally-offset @code{Fingerings} align along the Y axis when
they are within @code{FingeringColumn.snap-radius} of each other.
"
}

\relative {
  \set fingeringOrientations = #'(left)
  <cis''-1 a-1 e-1>4
  <ceses-1 a-1 e-1>4
}
