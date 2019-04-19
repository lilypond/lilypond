\version "2.21.0"

\header {
  texidoc="
Fermata over full-measure rests should invert when below and
be closer to the staff than other articulations."
}
\layout {
  ragged-right = ##t
}
\relative {
  R1
  a'1^"should be lower"
  R1^"should be higher"
  \break
  R1
  R1^"should be lower"
  a1^"should be higher"
  \break
  a1^"should be above fermata"
  R1^\fermata
  \break
  a1_"should be below fermata"
  R1_\fermata
}
