\version "2.17.6"

\header {
  texidoc = "Measures can be numbered sequentially by enclosing them with 
@code{\\startMeasureCount} and @code{\\stopMeasureCount}."
}

\layout {
  indent = 0
  ragged-right = ##t
}

\relative c' {
  \startMeasureCount
  \repeat unfold 5 {
    a4 b c d
  }
  \stopMeasureCount
  a'4 b c d
  \override Staff.MeasureCounter.count-from = #2
  \startMeasureCount
  \repeat unfold 4 {
    a4 b c d
  }
  \stopMeasureCount\startMeasureCount
  \revert Staff.MeasureCounter.count-from
  \clef bass
  \key fis \major
  \time 3/4
  \repeat unfold 3 {
    R2.
  }
  \stopMeasureCount
}

\layout {
  \context {
    \Staff
    \consists #Measure_counter_engraver
  }
}
