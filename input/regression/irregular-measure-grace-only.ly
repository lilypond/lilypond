\version "2.27.2"

\header {
  texidoc = "Adding @code{\\measure} around grace notes does not increment the
bar number, but still iterates the music and resets
@code{Timing@/.measureLength} based on the time signature.

The output should consist of two measures.  Measure@tie{}1 should contain only
a quarter note.  Measure@tie{}2 should contain two grace notes and four quarter
notes."
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
  \measure { \grace { c16 d16 } }
  4 4 4 4
}
