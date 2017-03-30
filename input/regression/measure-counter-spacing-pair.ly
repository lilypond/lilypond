\version "2.19.59"

\header {
  texidoc = "The @code{spacing-pair} property may be used to adjust
the horizontal positioning of @code{MeasureCounter} objects relative to
prefatory material.  In the following example, the count should be
aligned with the full-measure rests.
"
}

\layout {
  ragged-right = ##t
  \context {
    \Staff
    \consists #Measure_counter_engraver
  }
}

music = {
  \key bes \major
  \time 3/4
  R2.
}

{
  \startMeasureCount
  \music
  \override Staff.MeasureCounter.spacing-pair =
    #'(staff-bar. staff-bar)
  \override Staff.MultiMeasureRest.spacing-pair =
    #'(staff-bar . staff-bar)
  \music
  \override Staff.MeasureCounter.spacing-pair =
    #'(key-signature . staff-bar)
  \override Staff.MultiMeasureRest.spacing-pair =
    #'(key-signature . staff-bar)
  \music
  \override Staff.MeasureCounter.spacing-pair =
    #'(time-signature . staff-bar)
  \override Staff.MultiMeasureRest.spacing-pair =
    #'(time-signature . staff-bar)
  \music
  % time-signature is used on left
  \override Staff.MeasureCounter.spacing-pair =
    #'((left-edge time-signature) . time-signature)
  \override Staff.MultiMeasureRest.spacing-pair =
    #'((left-edge time-signature) . time-signature)
  \music
  \break
  % left-edge is used
  \override Staff.MeasureCounter.spacing-pair =
    #'((left-edge key-signature) . staff-bar)
  \override Staff.MultiMeasureRest.spacing-pair =
    #'((left-edge key-signature) . staff-bar)
  \music
  \stopMeasureCount
}
