\version "2.27.5"

\header {
  texidoc = "@code{beatStructure} is empty by default in senza-misura passages,
and it may be explicitly unset when a time signature is in force.  When it is
empty, it does not contribute a reason for beams to end at any point, though
beams may still be ended manually."
}

\layout {
  indent = 1
  ragged-right = ##t
  \context {
    \Score
    \senzaMisuraTimeSignatureX
  }
}

rhythmWithoutBeams = { c''16 16 8 16 16 16 8 16 16 16 8 16 16 }
rhythmWithBeams = { c''16[ 16 8] 16[ 16 16 8 16 16 16] 8[ 16 16] }

\score {
  \new Staff \with { instrumentName = "default" } {
    \rhythmWithoutBeams
  }
}

\score {
  \new Staff \with { instrumentName = \markup \column { man. beams } } {
    <>^\markup \typewriter "\unset Timing.beatStructure"
    \unset Timing.beatStructure
    \rhythmWithBeams
  }
}

\score {
  \new Staff \with { instrumentName = \markup \column { man. beams } } {
    \senzaMisura
    \rhythmWithBeams
    \section
  }
}

\score {
  \new Staff \with { instrumentName = \markup \column { auto. beams } } {
    <>^\markup \typewriter "\unset Timing.beatStructure"
    \unset Timing.beatStructure
    \rhythmWithoutBeams
  }
}

\score {
  \new Staff \with { instrumentName = \markup \column { auto. beams } } {
    \senzaMisura
    \rhythmWithoutBeams
    \section
  }
}
