\version "2.13.9"

\header {
  texidoc = "Moving the @code{Volta_engraver} to the @code{Staff}
context does not affect @code{InstrumentName} alignment."
}

testMusik =  \relative c'' {
  \repeat volta 2 {
    c1 \break
  } \alternative {
    { c1 \break }
    { c1 \break }
  }
}

\score {
  \new Staff \relative c' {
    \set Staff.shortInstrumentName = #"Instr."
    \repeat volta 2 {
      c1 \break
    }
    \alternative {
      { c1 \break }
      { c1 \break }
    }
  }
}
\layout {
  ragged-right = ##t
  short-indent = 5\mm
  \context {
    \Score
    \remove "Volta_engraver"
  }
  \context {
    \Staff
    \consists "Volta_engraver"
  }
}
