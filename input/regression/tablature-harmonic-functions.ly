\version "2.17.30"

\header {
  texidoc = "
Harmonics can be specified either by ratio or by fret number.
"
}

test = {
 e,4
 \harmonicByRatio #1/2  e,\6
 \harmonicByRatio #1/3  a,\5
 \harmonicByRatio #2/3  d,\4 |
 \harmonicByRatio #1/4 { g8\3 b\2 e'\1 b\2 < g b e >2 } |
 e,1 | % check whether tab note head is restored
 \harmonicByFret #12 e'4\1 ~
 \harmonicByFret #12 e'4\1 (
 \ottava #1
 \harmonicByFret #7 e'4\1)
 \harmonicByFret #5 e'8\1
 \ottava #2
 \harmonicByFret #4 < b\2 e'\1 >8 |
 \harmonicByFret #3 < g\3 b\2 e'\1 >4
 \harmonicByFret #2.7 < g\3 b\2 e'\1 >4
 \harmonicByFret #2.3 < g\3 b\2 e'\1 >4
 \harmonicByFret #2 < g\3 b\2 e'\1 >4 |
 \ottava #0
 e,1 | % check whether tab note head is restored
}

\paper {
  ragged-right = ##f
}

\score {
  <<
    \new Staff {
      \new Voice \with { \omit StringNumber } {
        \clef "treble_8"
        \test
      }
    }
    \new TabStaff {
      \new TabVoice {
        \test
      }
    }
  >>
}
