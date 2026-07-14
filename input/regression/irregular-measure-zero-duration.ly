\version "2.27.2"

\header {
  texidoc = "Adding @code{\\measure} to zero-duration music does not increment
the bar number, but still iterates the music and resets
@code{Timing@/.measureLength} based on the time signature.

The output should consist of two measures.  Measure@tie{}1 should contain only
a quarter note and a breathing sign.  Measure@tie{}2 should contain four
quarter notes."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
  }
}

\fixed c' {
  \set Timing.measureLength = #1/4
  f4
  \measure { \breathe }
  4 4 4 4
}
