\version "2.25.21"

\header {
  texidoc = "@code{\\partial} can create measures longer than the
length dictated by the time signature."
}

\new Score \with {
  barNumberVisibility = #all-bar-numbers-visible
  \override BarNumber.break-visibility = #all-visible
  %% Weird beaming should make this test more sensitive to regressions in
  %% calculating measurePosition.
  \time 3 3/8
} \fixed c' {
  \partial 8*4 8 8 8 8 | 8 8 8 | \partial 8*4 8 8 8 8 | 8 8 8 |
}
