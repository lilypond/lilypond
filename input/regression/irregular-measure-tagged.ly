\version "2.27.2"

\header {
  texidoc = "@code{\\measure} adapts when its music is modified by tag
operations.  Measure@tie{}2 should be irregular, having only five eighth
notes."
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
  \*6 c8 |
  \measure { f8 f8 \tag TagA d8 f8 f8 f8 }
  \*6 c8 |
}

\new Score { \removeWithTag TagA \music }
