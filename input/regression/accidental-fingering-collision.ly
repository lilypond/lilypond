\version "2.19.21"

\header {
  texidoc = "Horizontal @code{Fingering} grobs should not collide with
accidentals.
"
}

\relative {
  \time 2/4
  \set fingeringOrientations = #'(left)
  <a-3 cis-4> <a-3 cis!-4> |
}