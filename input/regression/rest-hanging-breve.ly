\version "2.21.0"

\header {
  texidoc = "Breve, longa, and maxima rests should hang down from
staff lines in one line staves, different staff space and font size."
}

music = {
  \omit Staff.TimeSignature
  \compressEmptyMeasures
  \override MultiMeasureRest.usable-duration-logs = #'(3)
  \override MultiMeasureRest.expand-limit = 127

  \time 381/16
  r8. r4. r2. r1. r\breve. r\longa. r\maxima.
  \time 1/8
  R8*127
}

<<
  \new Staff \music
  \new RhythmicStaff \music
  \new TabStaff \with {
    \tabFullNotation
  } \music
  \new CueVoice \music
>>
