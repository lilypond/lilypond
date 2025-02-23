\version "2.25.25"

\header {
  texidoc = "The @code{numbered} time-signature style prints a fraction."
}

#(ly:set-option 'warning-as-error #t)

\new Staff \with {
  \override TimeSignature.style = #'numbered
} \fixed c' {
  \time 4/4 d1

  \time 3/4 d2.

  \time 2/2 d1

  \time 16/128 d8

  \time 10/6 \tuplet 6/4 { d2. d2 d2. d2 }

  %% TODO: If it becomes possible to pass values like #'(2 . 1/2) to \time, this
  %% should be changed to do that instead of using \override.

  \once \override Timing.TimeSignature.fraction = #'(2 . 1/2)
  \time 4/1 d\longa

  \once \override Timing.TimeSignature.fraction = #'(2/3 . 2)
  \time 1/3 \tuplet 3/2 { e'4 d'4 }
}
