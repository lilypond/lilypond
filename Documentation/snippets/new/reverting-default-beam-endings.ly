\version "2.14.0"

\header {
  lsrtags = "rhythms"
  texidoc = "
To typeset beams grouped @code{3-4-3-2} one need only change the
beat structure:
"
  doctitle = "Reverting default beam endings"
}

\relative c'' {
  \time 12/8

  % Default beaming
  a8 a a a a a a a a a a a

  % Set new values for beam endings
  \set Score.beatStructure = #'(3 4 3 2)
  a8 a a a a a a a a a a a
}
