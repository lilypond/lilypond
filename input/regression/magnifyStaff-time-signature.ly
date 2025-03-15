\version "2.25.25"

\header {
  texidoc = "The @code{\\magnifyStaff} command should scale fractional and
number-over-note time signatures."
}

\paper {
  indent = 0
  line-width = 8 \cm
  ragged-right = ##f
  score-system-spacing = #'((padding . 3))
}

music = \fixed c' {
  \once \override Timing.TimeSignature.fraction = #'(7/4 . 2)
  \time 2/4
  s2

  \once \override Timing.TimeSignature.nested-fraction-orientation =
  #'horizontal
  \once \override Timing.TimeSignature.fraction = #'(41/7 . 8)
  \time 2/4
  s2

  \once \override Timing.TimeSignature.denominator-style = #'note
  \once \override Timing.TimeSignature.fraction = #'(9 . 1/6)
  \time 2/4
  s2
}

\new Staff { \magnifyStaff 0.5 \music }
\new Staff { \magnifyStaff 1.0 \music }
\new Staff { \magnifyStaff 2.0 \music }
