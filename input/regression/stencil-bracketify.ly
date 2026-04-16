\header {
  texidoc = "Testing brackets around a grob."
}

\version "2.27.0"

\relative c'' {
  \override Staff.TimeSignature.stencil = #(lambda (grob)
    (bracketify-stencil (ly:time-signature::print grob) X 0.1 0.2 0.1))
  \time 3/4 a4 a a

  \override Staff.TimeSignature.stencil = #(lambda (grob)
    (bracketify-stencil (ly:time-signature::print grob) Y 0.5 3 2 1))
  \time 2/4 a4 a
}
