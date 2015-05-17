\version "2.19.21"

\header {
  texidoc = "
Chord repetition handles \\relative mode: the repeated chords have
the same octaves as the original one.
"
}

{
  <c''' d'' g''>4^"absolute" q q q
  \relative { <c''' d, g>4^"relative" q q q }
}
