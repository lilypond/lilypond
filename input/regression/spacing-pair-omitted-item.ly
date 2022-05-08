\version "2.23.9"

\header {
  texidoc = "For @code{spacing-pair}, when an item matching a break align symbol
is omitted, the alignment falls back on later break align symbols in the list.
In this test, the measure counter should be centered using the right edge of the
key signature."
}

\new Score \with {
  \consists Measure_counter_engraver
} {
  \override Score.MeasureCounter.spacing-pair = #'((time-signature key-signature) . staff-bar)
  \omit Staff.TimeSignature
  \key cis \major
  \startMeasureCount
  c'1
  \stopMeasureCount
}
