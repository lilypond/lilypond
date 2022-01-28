\version "2.23.9"

\header {
  texidoc = "Trill spanners stop before the accidental
of the following note, if any."
}

{
  \override Score.SpacingSpanner.spacing-increment = 3.9
  c''2\startTrillSpan ceses''\stopTrillSpan
  c''2\startTrillSpan ceses''\stopTrillSpan\startTrillSpan
}
