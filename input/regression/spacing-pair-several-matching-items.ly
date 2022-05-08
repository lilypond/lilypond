\version "2.23.9"

\header {
  texidoc = "The @code{spacing-pair} property takes the combined extents
of all items having the given break align symbol into account.  In this
test, the centering of the measure counter should visibly happen with
the left point being on the right of the wide key signature.  The alignment
of the measure counter should be the same for both scores."
}

\layout {
  \context {
    \Score
    \consists Measure_counter_engraver
    \override MeasureCounter.spacing-pair = #'(key-signature . staff-bar)
  }
}

\paper {
  ragged-right = ##t
}

staffA =
\new Staff {
  \key g \major
  \startMeasureCount
  c'1
  \stopMeasureCount
}

staffB =
\new Staff {
  \key cis \major
  gis'1
}

<< \staffA \staffB >>
<< \staffB \staffA >>
