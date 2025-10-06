\version "2.25.30"

\header {
  texidoc = "@code{\\senzaMisuraTimeSignatureX} and
@code{\\senzaMisuraTimeSignatureOff} control the creation of an X sign when
@code{TimeSignature@/.time-signature} is not set.  The middle bar of the middle
staff should contain an X-shaped sign and a quarter note."
}

music = \fixed c' {
  d1
  \once \override Staff.TimeSignature.time-signature = ##f
  \time 1/4
  d4
  \time 2/2
  d1
}

\new Score {
  \new Staff \with {
    instrumentName = "default"
  } \music
}

\new Score \with { \senzaMisuraTimeSignatureX } {
  \new Staff \with { instrumentName = "X" } \music
}

\new Score \with { \senzaMisuraTimeSignatureX } {
  \new Staff \with { instrumentName = "Off" } {
    \senzaMisuraTimeSignatureOff
    \music
  }
}
