\header {
  texidoc ="The part combiner has an internal option to set the range
of intervals that may be combined into chords."
}

\version "2.19.16"

\layout { ragged-right = ##t }

vone = \relative e' {
  d4 e f fisis | g geses b' bisis | b2 beses
}

vtwo = \relative e' {
  e4 e e e | e eisis d deses | c2 cisis
}

customcombine =
#(define-music-function (parser location part1 part2) (ly:music? ly:music?)
   (make-part-combine-music parser (list part1 part2) #f '(2 . 12)))

comm = { s1_"apart" s1_"chords" s1_"apart" }

\new Staff <<
  \customcombine \vone \vtwo
  \comm
>>
