\version "2.16.0"
\paper { ragged-right = ##t }

\header {
  texidoc = "@code{InstrumentName} is reasonable positioned even for unusual
system-start-delimiters.

Below, the @code{instrumentName} neither collides with the
@code{SystemStartBracket} nor moves to far to the left."
}


\layout {
  \context {
    \StaffGroup
    \override InstrumentName.self-alignment-X = #RIGHT
    instrumentName = "StaffGroup"
  }
}

\score {
  \new StaffGroup << R1 R >>
  \layout {
    \override Score.SystemStartBar.collapse-height = 50
  }
}

\score {
  \new StaffGroup
    \with { \override SystemStartBracket.X-offset = 10 }
    << R1 R >>
}
