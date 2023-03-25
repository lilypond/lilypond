\version "2.25.3"

\header {
  texidoc = "It is possible to define contexts that, when instantiated,
take the normal place of @code{Score}.

This test should show a score with proportional notation and bigger
note heads."
}

\layout {
  \context {
    \Score
    \name ProportionalScore
    \alias Score
    proportionalNotationDuration = \musicLength 4
  }
  \inherit-acceptability ProportionalScore Score
}

\new ProportionalScore {
  \override Score.NoteHead.font-size = 2
  c'1 2 4 8 16 32 32
}
