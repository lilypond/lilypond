\version "2.23.7"

\header {
  texidoc = "The @code{\enablePolymeter} command turns on polymetric
notation, making time signatures independent between staves."
}

\layout {
  \enablePolymeter
}

<<
 \new Staff { \time 4/4 c'1 | d' | f' }
  \new PianoStaff <<
    \new Staff { \time 3/4 c'2. | 4 2 | 2 4 | 4 4 4 }
    \new Dynamics { \time 3/4 s2.\p\< s\> s\! s }
    \new Staff { \time 3/4 c'2. | 4 2 | 2 4 | 4 4 4 }
  >>
>>
