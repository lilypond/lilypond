\version "2.11.59"

\header {
  texidoc = "The left bound text of dynamic text spanners is
horizontally aligned with the left edge of a note column."
}

\relative c'' {
  \crescTextCresc
  c1\<
  e2 f\!
  \dimTextDecresc
  e2\> d
  c b\!
}
