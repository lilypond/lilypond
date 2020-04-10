\version "2.21.0"

\header {
  texidoc = "Test for vertical positions of merged rests in magnified staves."
}

music = {
  R1
  R1*2
  \compressEmptyMeasures
  R1*7
  R1*11
  \time 4/2
  R\breve
  r\breve
  r1 r2 r4 r8 r
}

<<
  \new Staff \with {
    \magnifyStaff #3/4
    \consists "Merge_rests_engraver"
  } << \music \\ \music >>
  \new Staff \with {
    \magnifyStaff #4/3
    \consists "Merge_rests_engraver"
  } << \music \\ \music >>
>>
