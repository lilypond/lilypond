\version "2.17.15"

\header {
  texidoc = "Percent repeats are also centered when there is a grace note in a parallel staff. "
}

\layout {
  ragged-right =##t
}

\relative c' <<
  \new Staff { \repeat percent 3 c1} 
  \new Staff { c1 c \grace b8 c1  }
>>
