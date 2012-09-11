\version "2.16.0"

\header {
  texidoc = "Staves, RhythmicStaves, TabStaves and DrumStaves
  with percent repeats are not suppressed."
}

<<
  \new Staff { c''1 c'' \break c'' c'' }
  \new Staff \repeat percent 4 { c'1 }
  \new TabStaff \repeat  percent 4 { c1 }
  \new DrumStaff \drummode { \repeat percent 4 { hh1 } }
  \new RhythmicStaff \repeat percent 4 { c'1 }
>>

\layout {
  \context { \Staff \RemoveEmptyStaves }
  \context { \RhythmicStaff \RemoveEmptyStaves }
  \context { \DrumStaff \RemoveEmptyStaves }
  \context { \TabStaff \RemoveEmptyStaves }
}
