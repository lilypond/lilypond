\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="Coda marks are formatted with @code{codaMarkFormatter},
which the user can override.  Rehearsal marks and coda marks are
sequenced independently."
}

piece = \fixed c' {
  \time 3/4 % for comparison of numbers
  r4 \codaMark \default r4 r4 |
  \mark 23 R2. |
  \codaMark \default R2. |
  \codaMark 96 R2. |
}

\new Score {
  \new Staff \with { instrumentName = "default" } {
    \piece
  }
}

\new Score \with {
  codaMarkFormatter = #format-varcoda-mark
} {
  \new Staff \with {
    instrumentName = \markup \column { "varcoda" }
  } {
    \piece
  }
}
