\version "2.27.5"

\header {
  texidoc = "
Prefatory elements in the initial music of a percent repeat shall be shifted to
occur before grace notes in parallel music."
}

\layout {
  ragged-right =##t
}

\relative <<
  \new Staff { \%3 { c'2 \clef "bass" c,,2 \clef "treble" } }
  \new Staff { c''1 \grace b8 c1 \grace b8 c1 }
>>
