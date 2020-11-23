\header {
  texidoc = "Stencils are copied before moved for Balloons instead of
modified.  In the test, the @code{point-stencil} in the second system
should not inherit the extent from the @code{null-markup} in the first
and the bar should be much shorter."
}

\version "2.23.0"

\new Voice \with { \consists "Balloon_engraver" }
{
  \balloonGrobText #'NoteHead #'(10 . -2) \markup \null
  c'1
}

{
  \override NoteHead.stencil = #point-stencil
  c'1
}
