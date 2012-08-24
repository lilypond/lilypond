
\header {
  texidoc ="If the part-combiner shows two separate voices, multi-measure rests
  are supposed to use the same settings as @code{\voiceOnce} and @code{\voiceTwo}.
"
}

\layout { ragged-right = ##t }

\version "2.16.0"

mI = \relative c'' {
  \partcombineApart
  c2 c |
  R1 |
  c1
}
mII = \relative c' {
  R1 |
  c2 c |
  c1
}

\score {
  \new Staff \partcombine \mI \mII
}
