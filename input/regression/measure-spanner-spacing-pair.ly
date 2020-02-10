\version "2.21.0"

\header {
  texidoc="
The ends of measure spanners may be aligned in various ways.
"
}

\new Staff {
  \key cis \major
  \tweak text #"2x"
  \startMeasureSpanner cis''1\stopMeasureSpanner
  \break
  \time 3/4
  \clef bass
  \tweak text #"100x"
  \tweak spacing-pair #'(key-signature . clef)
  \startMeasureSpanner cis2.\stopMeasureSpanner
  \break
  \time 4/4
  \clef treble
  \tweak text \markup \char ##x221E
  \tweak spacing-pair #'(time-signature . clef)
  \startMeasureSpanner cis''1\stopMeasureSpanner
}

\layout {
  \context {
    \Staff
    \consists "Measure_spanner_engraver"
  }
}
