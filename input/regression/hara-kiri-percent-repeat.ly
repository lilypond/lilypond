\version "2.25.35"

\header {
  texidoc = "Staves, RhythmicStaves, TabStaves and DrumStaves
  with percent repeats are not suppressed."
}

<<
  \new Staff { c''1 c'' \break c'' c'' }
  \new Staff \%4 c'1
  \new TabStaff \%4 c1
  \new DrumStaff \drummode { \%4 hh1 }
  \new RhythmicStaff \%4 c'1
>>

\layout {
  \context { \Staff \RemoveEmptyStaves }
  \context { \RhythmicStaff \RemoveEmptyStaves }
  \context { \DrumStaff \RemoveEmptyStaves }
  \context { \TabStaff \RemoveEmptyStaves }
}
