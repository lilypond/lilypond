\version "2.27.2"

\header {
  texidoc = "@code{\\measure} may be used in consecutive measures.  In this
test, measures@tie{}2 and@tie{}3 are irregular, and the measure length reverts
to 3/4 in measure@tie{}4."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
  }
}

\new Score \fixed c' {
  \time 3/4
  c2.
  \measure d1
  \measure e\breve
  f2.
}
