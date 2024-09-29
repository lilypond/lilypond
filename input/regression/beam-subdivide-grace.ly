\version "2.25.27"

\header {
  texidoc = "Beamed grace notes are subdivided."
}

\fixed c'' {
  \set subdivideBeams = ##t
  r2 \grace { \repeat unfold 15 c64 } c4 r4
}
