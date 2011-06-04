\version "2.14.0"


\header {
  texidoc ="Wait for the next real note for part-combine texts (i.e. don't
print part-combine texts on rests). This is needed because the part-combiner
needs an override if one voice has a full-bar rest while the other has some
rests and then a solo."
}

\layout { ragged-right = ##t }

mI = \relative c'' {
  \set Score.partCombineTextsOnNote = ##t
  g4 \partcombineSoloI r4 c2 |
  \partcombineSoloII R1*2 |
}
mII = \relative c' {
  c4 r2. |
  r2 r4 c4 |
  R1 |
}

\score {
  \new Staff \partcombine \mI \mII
}
