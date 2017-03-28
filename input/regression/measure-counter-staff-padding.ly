\version "2.19.59"

\header {
  texidoc = "The @code{staff-padding} property may be used to adjust
the distance of @code{MeasureCounter} objects from the staff.  The
following example uses @code{staff-padding} to align the count
vertically.
"
}

\layout {
  \context {
    \Staff
    \consists #Measure_counter_engraver
  }
}

music = {
  \startMeasureCount
  c''2 c''
  c'''2 c'''
  \bar "||"
  \stopMeasureCount
}

{
  \music
  \override Staff.MeasureCounter.staff-padding = 4
  \music
}
