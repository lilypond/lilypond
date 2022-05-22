\version "2.23.9"

\header {
  texidoc = "@code{\\partial} can create measures longer than the
length dictated by the time signature."
}

\new Score \with {
  barNumberVisibility = #all-bar-numbers-visible
  \override BarNumber.break-visibility = #all-visible
} {
  \partial 1. c''2 2 2 | 1 | \partial 1. 2 2 2 | 1 |
}
