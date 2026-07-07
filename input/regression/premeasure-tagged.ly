\version "2.27.2"

\header {
  texidoc = "@code{\\premeasure} adapts when its music is modified by tag
operations.  There should be three notes before bar@tie{}1."
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
  \premeasure { f8 \tag TagA d8 f8 f8 }
  \*6 c8 |
}

\new Score { \removeWithTag TagA \music }
