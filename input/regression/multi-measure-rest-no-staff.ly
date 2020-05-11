\version "2.21.2"

\header {
  texidoc = "Multi measure rests don't segfault when there is no staff symbol."
}

\layout {
  \context {
    \type Engraver_group
    \name GlobalRests
    \consists Multi_measure_rest_engraver

  }
  \context {
    \Score
    \accepts GlobalRests
  }
}

\score {
  <<
    \new GlobalRests { R1 }
  >>
}