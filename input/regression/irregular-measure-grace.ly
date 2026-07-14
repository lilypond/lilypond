\version "2.27.2"

\header {
  texidoc = "@code{\\measure} can begin with grace notes."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
  }
}

music = \fixed c' {
  \time 6/8
  \*6 f8 |
  \measure { \grace { d16 e16 } \*5 f8 }
  \*6 f8 |
}

\new Score { \music }
