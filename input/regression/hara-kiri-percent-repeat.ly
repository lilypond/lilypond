\header {
  texidoc = "Staves with percent repeats are not killed."
}

\version "2.11.53"

<<
 \new Staff { c''1 c'' \break c'' c'' }
 \new Staff \repeat percent 4 { c'1 }
>>                          

\layout {
 \context {
   \RemoveEmptyStaffContext
 }
}
