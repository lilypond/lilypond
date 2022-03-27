\version "2.23.7"

\header {
  texidoc = "@code{\enablePolymeter} can also be used with MIDI output.
This test should not produce bar check errors."
}

\score {
  \midi {
    \enablePolymeter
  }
  <<
    \new Staff { \time 3/4 c'2. | 4 2 | 2 4 | 4 4 4 }
    \new Staff { \time 4/4 c'1 | d' | f' }
  >>
}
