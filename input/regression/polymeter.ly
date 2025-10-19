\version "2.25.30"

\header {
  texidoc = "The @code{\enablePerStaffTiming} command makes @code{measureLength}
and other timing properties independent between staves."
}

\layout {
  \enablePerStaffTiming
}

<<
 \new Staff { \time 4/4 c'1 | d' | f' }
  \new PianoStaff <<
    \new Staff { \time 3/4 c'2. | 4 2 | 2 4 | 4 4 4 }
    \new Dynamics { \time 3/4 s2.\p\< s\> s\! s }
    \new Staff { \time 3/4 c'2. | 4 2 | 2 4 | 4 4 4 }
  >>
>>
