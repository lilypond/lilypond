\version "2.19.21"

\header {
  texidoc = "The @code{break-overshoot} property sets the amount that
a broken spanner `sticks out' of its bounds.

For broken beams and broken tuplet brackets, the bounds are given by the
prefatory matter on the left and/or the rightmost column on the right.  For
broken horizontal brackets, the bounds are the leftmost and/or rightmost
columns; for measure spanners, the left and/or right edge of the staff."
}

{
  \override Beam.break-overshoot = #'(1.0 . 2.0)
  \override Beam.breakable = ##t
  c''2.. c''8[ \time 2/2 \key e\major \break
  \repeat unfold 8 c''8 \break
  c''8] c''2..
}

{
  \override TupletBracket.break-overshoot = #'(1.0 . 2.0)
  \tuplet 3/2 { c''1. \time 2/2 \key e\major \break
  cis''1. \break
  c''1. }
}

{
  \override HorizontalBracket.break-overshoot = #'(1.0 . 2.0)
  c''1\startGroup \time 2/2 \key e\major \break
  cis''1 \break
  c''1
}

{
  \override Staff.MeasureSpanner.break-overshoot = #'(1.0 . 2.0)
  \startMeasureSpanner c''1 \time 2/2 \key e\major \break
  cis''1 \break
  c''1 \stopMeasureSpanner
}

\layout {
  ragged-right = ##t

  \context {
    \Staff
    \consists "Measure_spanner_engraver"
  }
  \context {
    \Voice
    \consists "Horizontal_bracket_engraver"
  }
}
