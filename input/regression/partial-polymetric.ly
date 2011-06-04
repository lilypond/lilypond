\version "2.14.0"

\header {
  texidoc = "@code{\\partial} works with polymetric staves."
}

\score {
  \relative c' <<
    \new Staff {
      \partial 4
      c4 |
      c4 c c c |
    }
    \new Staff {
      \time 3/4
      \partial 2
      c4 c |
      c4 c c |
    }
  >>
  \layout {
    \context {
      \Score
      \remove "Timing_translator"
      \remove "Default_bar_line_engraver"
    }
    \context {
      \Staff
      \consists "Timing_translator"
      \consists "Default_bar_line_engraver"
    }
  }
}
