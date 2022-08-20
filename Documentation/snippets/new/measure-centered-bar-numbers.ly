\version "2.23.9"

\header {
  lsrtags = "tweaks-and-overrides"

  texidoc = "
For film scores, a common convention is to center bar
numbers within their measure.  This is achieved through setting
the @code{centerBarNumbers} context property to true.  When this
is used, the type of the bar number grobs is @code{CenteredBarNumber}
rather than @code{BarNumber}.

This example demonstrates a number of settings: the centered bar
numbers are boxed and placed below the staves.
"

  doctitle = "Measure-centered bar numbers"
}


\layout {
  \context {
    \Score
    centerBarNumbers = ##t
    barNumberVisibility = #all-bar-numbers-visible
    \override CenteredBarNumber.stencil
      = #(make-stencil-boxer 0.1 0.25 ly:text-interface::print)
    \override CenteredBarNumberLineSpanner.direction = #DOWN
  }
}

\new StaffGroup <<
  \new Staff \relative c' {
    d4-. f8( e d4) bes'-> |
    d,-. f8( e d4) cis'-> |
    g-. f8( d e4) g-> |
    a,1-> |
  }
  \new Staff \relative c {
    \clef bass
    d4 f8 e d2~ |
    4 f8 e d2~ |
    4 4 2 |
    a1 |
  }
>>
