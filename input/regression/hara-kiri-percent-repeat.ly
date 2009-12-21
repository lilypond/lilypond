\version "2.13.10"

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
  \context { \RemoveEmptyStaffContext }
  \context { \RemoveEmptyRhythmicStaffContext }
  \context { \RemoveEmptyDrumStaffContext }
  \context { \RemoveEmptyTabStaffContext }
  }

