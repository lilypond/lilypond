\version "2.23.3"

\header {
  texidoc = "When a measure counter extends over a compressed
multi-measure rest, it displays the full measure range.  By
default, the two measure numbers in the range are dash-separated;
this is configurable."
}

\new Staff \with {
  \consists Measure_counter_engraver
}
\compressMMRests {
  \startMeasureCount
  c1
  R1*5
  c1
  \stopMeasureCount
  \override Staff.MeasureCounter.number-range-separator = "…"
  \startMeasureCount
  R1*7
  \stopMeasureCount
}
