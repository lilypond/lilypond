\version "2.25.25"

\header {
  texidoc = "It is possible to override @code{TimeSignature@/.fraction} to
change the printed time signature without changing related context properties.

The marginal labels show the values of the @code{style} and
@code{denominator-@/style} properties in each case."
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
    instrumentName = \markup \column {
      \typewriter "C"
      "(default)"
    }
  } \music

  \new Staff \with {
    \override TimeSignature.style = #'numbered
    instrumentName = \markup \column {
      \typewriter "numbered"
      "(default)"
    }
  } \music

  \new Staff \with {
    \override TimeSignature.style = #'numbered
    \override TimeSignature.denominator-style = #'none
    instrumentName = \markup \column {
      \typewriter "numbered"
      \typewriter "none"
    }
  } \music

  \new Staff \with {
    \override TimeSignature.style = #'numbered
    \override TimeSignature.denominator-style = #'note
    instrumentName = \markup \column {
      \typewriter "numbered"
      \typewriter "note"
    }
  } \music

  \new Staff \with {
    \override TimeSignature.style = #'single-number
    instrumentName = \markup \column {
      \typewriter "single-number"
      "(default)"
    }
  } \music
>>
