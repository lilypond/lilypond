\version "2.25.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "In a measure beginning with grace notes and cadenza
material, if a bar line is added at the end of the cadenza, its bar
number is parenthesized even though the cadenza does not advance
@code{measurePosition}.

The first staff tests this at the start of the first and following
measures of a piece.  The second staff tests this before an initial
anacrusis."
}

\layout {
  \context {
    \Score
    \override BarNumber.break-visibility = #all-visible
    barNumberVisibility = #all-bar-numbers-visible
    caesuraType = #'((underlying-bar-line . ";"))
  }
}

\new Score \fixed c'' {
  \repeat unfold 2 {
    \grace d16
    \cadenzaOn
    e8 r4
    \cadenzaOff
    \caesura
    c1
  }
}

\new Score \fixed c'' {
  \override Score.BarNumber.break-visibility = #all-visible
  \set Score.barNumberVisibility = #all-bar-numbers-visible
  \partial 4
  \grace d16
  \cadenzaOn
  e8 r4
  \cadenzaOff
  \caesura
  c4
  \caesura
  c2.
}
