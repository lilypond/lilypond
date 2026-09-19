\version "2.27.5"

\header {
  texidoc = "
Initial grace notes of different lengths in different staves must not
produce redundant or misplaced clefs and key signatures."
}

\layout { ragged-right = ##t }

<<
  \new Staff { \key d \major \grace c''8 c''1 }
  \new Staff { \clef bass \key d \major \grace c16 c1 }
>>
