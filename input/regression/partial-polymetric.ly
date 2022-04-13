\version "2.23.8"

\header {
  texidoc = "@code{\\partial} works with polymetric staves."
}

\score {
  \relative <<
    \new Staff {
      \partial 4
      c'4 |
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
    }
    \context {
      \Staff
      \consists "Timing_translator"
    }
  }
}
