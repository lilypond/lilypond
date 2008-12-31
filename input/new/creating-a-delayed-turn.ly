\version "2.12.0"

\header {
  lsrtags = "expressive-marks, editorial-annotations, tweaks-and-overrides"
  texidoc = "Creating a delayed turn, where the lower note of the
  turn uses the accidental, requires several overrides.  The
  @code{outside-staff-priority} property must be set to #f, as otherwise
  this would take precedence over the @code{avoid-slur property}.  The
  value of @code{halign} is used to position the turn horizontally."

  doctitle = "Creating a delayed turn"
}

\relative c'' {
  \once \override TextScript #'avoid-slur = #'inside
  \once \override TextScript #'outside-staff-priority = ##f
  c2(^\markup \tiny \override #'(baseline-skip . 1) {
    \halign #-4
    \center-column {
      \sharp
      \musicglyph #"scripts.turn"
    }
  }
  d4.) c8
}
