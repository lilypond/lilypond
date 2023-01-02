\version "2.24.1"

\header {
  texidoc = "The @code{alternativeNumberingStyle} context property
controls the bar-numbering scheme and style in volta repeat
alternatives."
}

\layout {
  \override Score.BarNumber.break-visibility = #all-visible
}

music = \fixed c' {
  \repeat volta 4 {
    g1
    \alternative {
      \volta 1 a
      \volta 2,3 b
      \volta 4 d'
    }
  }
  e'
}

\new Score {
  \new Staff \with { instrumentName = "(default)" } \music
}

\new Score \with { \unset alternativeNumberingStyle } {
  \new Staff \with { instrumentName = "(unset)" } \music
}

\new Score \with { alternativeNumberingStyle = ##f } {
  \new Staff \with { instrumentName = "#f" } \music
}

\new Score \with { alternativeNumberingStyle = #'numbers } {
  \new Staff \with { instrumentName = "numbers" } \music
}

\new Score \with { alternativeNumberingStyle = #'numbers-with-letters } {
  \new Staff \with {
    instrumentName = \markup \column { "numbers-" "with-letters" }
  } \music
}
