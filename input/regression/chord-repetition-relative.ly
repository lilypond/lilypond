\version "2.13.9"

\header {
  texidoc = "
Chord repetition handles \\relative mode: the repeated chords have
the same octaves as the original one.
"
}

{
  <c''' d'' g''>4^"absolute" q q q
  \relative c' { <c'' d, g>4^"relative" q q q }
}