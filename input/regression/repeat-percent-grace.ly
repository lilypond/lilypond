\version "2.19.21"

\header {
  texidoc = "Percent repeats are also centered when there is a grace note in a parallel staff. "
}

\layout {
  ragged-right =##t
}

\relative <<
  \new Staff { \repeat percent 3 c'1} 
  \new Staff { c1 c \grace b8 c1  }
>>
