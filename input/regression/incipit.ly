\version "2.21.0"

\header {
  texidoc = "Incipits can be printed using an @code{InstrumentName}
grob.  In both lines the instrument name should appear left-aligned,
and there should be a small gap between the incipit and the staff."
}

\score {
  \new Staff \with {
    instrumentName = "Instrument"
    shortInstrumentName = "Instr."
  } {
    \incipit #1 { \clef "petrucci-c1" c'4 d' e' f' }
    c'4 d' e' f' \break g'1
  }
  \layout {
    \override Staff.InstrumentName.self-alignment-X = #LEFT
    indent = 6\cm
    short-indent = 6\cm
    incipit-width = 3\cm
  }
}
