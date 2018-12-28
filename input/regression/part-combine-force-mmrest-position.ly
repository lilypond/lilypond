
\header {
  texidoc ="If the part-combiner shows two separate voices, multi-measure rests
  are supposed to use the same settings as @code{\\voiceOnce} and @code{\\voiceTwo}.
"
}

\layout { ragged-right = ##t }

\version "2.21.0"

mI = \relative c'' {
  \partCombineApart
  c2 c |
  R1 |
  c1
}
mII = \relative {
  R1 |
  c'2 c |
  c1
}

\score {
  \new Staff \partCombine \mI \mII
}
