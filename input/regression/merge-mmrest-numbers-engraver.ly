\version "2.21.6"

\header {
  texidoc = "Test for merging rest numbers using the
@code{Merge_mmrest_numbers_engraver}.  The upper staff is the new
default with the engraver enabled while the second one is the old
default resulting in collisions.  The final staff demonstrates the
additional use of @code{Merge_rests_engraver}."
}

voice = {
  R1*7
  \override MultiMeasureRestNumber.text = \markup \text \huge "seven"
  R1*7
}
staff = <<
  \voice \\ \voice
>>

\compressMMRests
<<
  \new Staff \with {
    instrumentName = "default"
  } {
    \staff
  }
  \new Staff \with {
    instrumentName = "collision"
    \remove "Merge_mmrest_numbers_engraver"
  } {
    \staff
  }
  \new Staff \with {
    instrumentName = "merge"
    \consists "Merge_rests_engraver"
  } {
    \staff
  }
>>
