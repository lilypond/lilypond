\version "2.25.25"

\header {
  texidoc = "It is possible to override @code{TimeSignature@/.fraction} to
change the printed time signature without changing related context properties."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  indent = 2 \cm

  \context {
    \Staff
    \remove Clef_engraver
  }
}

music = \fixed c' {
  \once \override Timing.TimeSignature.fraction = 2/2
  \time 2/4
  s2

  \once \override Timing.TimeSignature.fraction = #'(0 . 16/127)
  \time 2/4
  s2

  \once \override Timing.TimeSignature.fraction = #'(3.14 . -4)
  \time 2/4
  s2

  \once \override Timing.TimeSignature.fraction = #'(2/3 . 0)
  \time 2/4
  s2
}

<<
  \new Staff \with {
    \override TimeSignature.style = #'C
    instrumentName = \markup \typewriter "C"
  } \music

  \new Staff \with {
    \override TimeSignature.style = #'numbered
    instrumentName = \markup \typewriter "numbered"
  } \music

  \new Staff \with {
    \override TimeSignature.style = #'single-number
    instrumentName = \markup \typewriter "single-number"
  } \music
>>
