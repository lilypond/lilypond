\header {
  texidoc ="The part combiner has an option to set the range of
differences in steps between parts that may be combined into chords."
}

\version "2.19.16"

\layout { ragged-right = ##t }

vone = \relative e' {
  d4 e f fisis | g geses b' bisis | b2 beses
}

vtwo = \relative e' {
  e4 e e e | e eisis d deses | c2 cisis
}

comm = { s1_"apart" s1_"chords" s1_"apart" }

\new Staff <<
  \partcombine #'(2 . 12) \vone \vtwo
  \comm
>>
