\version "2.21.0"


\header {
  texidoc ="Wait for the next real note for part-combine texts (i.e. don't
print part-combine texts on rests). This is needed because the part-combiner
needs an override if one voice has a full-bar rest while the other has some
rests and then a solo."
}

\layout { ragged-right = ##t }

mI = \relative {
  \set Score.partCombineTextsOnNote = ##t
  g'4 \partCombineSoloI r4 c2 |
  \partCombineSoloII R1*2 |
}
mII = \relative {
  c'4 r2. |
  r2 r4 c4 |
  R1 |
}

\score {
  \new Staff \partCombine \mI \mII
}
