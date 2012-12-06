\version "2.17.0"

\header {
  texidoc = "Horizontal @code{Fingering} grobs should not collide with
accidentals.
"
}

\relative c' {
  \time 2/4
  \set fingeringOrientations = #'(left)
  <a-3 cis-4> <a-3 cis!-4> |
}