\version "2.23.10"

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
    % FIXME: the presence of (dead) bar numbers disturbs spacing, causing a progerror
    \remove Bar_number_engraver
  }
}

\score {
  <<
    \new GlobalRests { R1 }
  >>
}
