\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This tests @emph{D.S. al Fine} form and how it unfolds."
}

piece = \new Voice \fixed c' {
  g1
  \repeat segno 2 {
    a1
    \volta 2 \fine
    \volta 1 b1
  }
}

\new Score {
  \new Staff \with { instrumentName = "default" } \piece
}

\new Score \with {
  dalSegnoTextFormatter = #format-dal-segno-text-brief
} {
  \new Staff \with { instrumentName = "brief" } \piece
}

\new Score {
  \new Staff \with { instrumentName = "unfolded" } \unfoldRepeats \piece
}
