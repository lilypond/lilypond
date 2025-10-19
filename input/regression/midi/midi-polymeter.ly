\version "2.25.30"

\header {
  texidoc = "@code{\enablePerStaffTiming} can also be used with MIDI output.
This test should not produce bar check errors."
}

\score {
  \midi {
    \enablePerStaffTiming
  }
  <<
    \new Staff { \time 3/4 c'2. | 4 2 | 2 4 | 4 4 4 }
    \new Staff { \time 4/4 c'1 | d' | f' }
  >>
}
