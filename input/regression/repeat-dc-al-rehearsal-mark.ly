\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This tests @emph{D.C. al Coda} form, but with a rehearsal
mark where the Coda label would normally be.  The @emph{D.C.}
instructions refer to the rehearsal mark."
}

piece = \new Voice \fixed c' {
  \repeat segno 2 {
    g1
    \alternative {
      { a1 | 1 }
      <>
    }
  }
  \mark \default
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
