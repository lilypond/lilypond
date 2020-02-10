\version "2.21.0"

\header {
  texidoc="
Measure spanners can span single and multiple measures.  They
may be texted or untexted and hold markups.
"
}

\new Staff {
  \startMeasureSpanner c''1\stopMeasureSpanner
  \bar "||"
  \tweak text #"foo"
  \startMeasureSpanner c''1\stopMeasureSpanner
  \bar "||"
  \tweak text #"bar" \startMeasureSpanner c''1
  c''1
  c''1\stopMeasureSpanner
  \bar "||"
  \tweak text \markup \ellipse "4x"
  \startMeasureSpanner c''1\stopMeasureSpanner
  \bar "||"
}

\layout {
  \context {
    \Staff
    \consists "Measure_spanner_engraver"
  }
}
