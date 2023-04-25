\version "2.25.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "Each staff contains the same music, but with different
values of @code{alternativeNumberingStyle}.  The body of the repeat is
a cadenza.  The bar number at the start of each alternative should be
parenthesized."
}

\layout {
  \context {
    \Score
    \override BarNumber.break-visibility = #all-visible
    barNumberVisibility = #all-bar-numbers-visible
  }
}

music = \fixed c' {
  \repeat volta 2 {
    \cadenzaOn r4 \fermata \cadenzaOff
    \alternative {
      \volta 1 { \bar ";" a1 | }
      \volta 2 { b1 | }
    }
  }
}

\new Score \music
\new Score \with { alternativeNumberingStyle = #'numbers } \music
\new Score \with { alternativeNumberingStyle = #'numbers-with-letters } \music
