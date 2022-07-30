\version "2.23.12"

\header {
  texidoc = "This tests the calculation of the anchor point on a
@emph{mensurstrich} bar line.

The top staff has no span bar.  A rehearsal mark should appear over an
invisible bar line between the two notes.

The lower pair of staves has a span bar of exaggerated width.  A
rehearsal mark should appear centered over it."
}

\layout {
  \context {
    \Score
    \override BarLine.hair-thickness = 50
  }
}

music = \fixed c' {
  c4
  \mark 21
  \bar "-span|"
  b4
}

\score {
  \new Staff \music
}

\score {
  \new StaffGroup <<
    \new Staff \music
    \new Staff \music
  >>
}
