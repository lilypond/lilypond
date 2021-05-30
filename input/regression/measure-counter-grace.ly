\version "2.23.3"

\header {
  texidoc = "Measure counts are not confused by grace notes."
}

\new Score \with {
  \consists Measure_counter_engraver
}
{
  \startMeasureCount
  \grace { a8 b }
  c'1
  \grace { c'8 d' }
  e'1
  \stopMeasureCount
}
