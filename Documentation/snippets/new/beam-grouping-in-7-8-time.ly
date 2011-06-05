\version "2.14.0"

\header {
  lsrtags = "rhythms"
  texidoc = "
There is no default beat structure specified for 7/8 time,
so if automatic beams are required the structure must be specified.  For
example, to group all beams 2-3-2 in 7/8 time, specify the
beat structure to be (2 3 2):
"
  doctitle = "Beam grouping in 7/8 time"
}

\relative c'' {
  \time 7/8
  % rhythm 2-3-2
  a8 a a a a a a
  \set Score.beatStructure = #'(2 3 2)
  a8 a a a a a a
}
