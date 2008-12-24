\header {
  texidoc = "Staves with percent repeats are not killed."
}

\version "2.12.0"

<<
 \new Staff { c''1 c'' \break c'' c'' }
 \new Staff \repeat percent 4 { c'1 }
>>                          

\layout {
 \context {
   \RemoveEmptyStaffContext
 }
}
