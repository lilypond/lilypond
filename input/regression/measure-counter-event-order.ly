\version "2.23.3"

\header {
  texidoc = "@code{\startMeasureCount} and @code{\stopMeasureCount} coming
in the same time step in this order do not cause a warning."
}

\layout {
  \context {
    \Score
    \consists Measure_counter_engraver
  }
}

{ \startMeasureCount c'1 \startMeasureCount \stopMeasureCount 1 \stopMeasureCount }

<<
  { c'1 \startMeasureCount c'1 \stopMeasureCount }
  { \startMeasureCount c'1 \stopMeasureCount 1 }
>>
