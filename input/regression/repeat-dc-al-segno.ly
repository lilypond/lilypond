\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This tests @emph{D.C.} al Coda form, but with a segno where
the Coda label would normally be.  The @emph{D.C.} instructions refer
to the segno."
}

piece = \new Voice \fixed c' {
  \repeat segno 2 {
    f1
    \alternative {
      { g1 | g }
      <>
    }
  }
  \repeat segno 2 {
    a1
  }
  b1
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
