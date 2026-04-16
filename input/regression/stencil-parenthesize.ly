\header {
  texidoc = "Testing parentheses around a grob."
}

\version "2.27.0"

\relative c'' {
  \override Staff.TimeSignature.stencil = #(lambda (grob)
    (parenthesize-stencil (ly:time-signature::print grob) 0.1 0.4 0.4 0.1))
  \time 3/4 a4 a a

  \override Staff.TimeSignature.stencil = #(lambda (grob)
    (parenthesize-stencil (ly:time-signature::print grob) 0.5 2 3 2 1))
  \time 2/4 a4 a
}
