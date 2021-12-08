\version "2.23.6"

\header {
  texidoc = "@code{stencil-align-dir-y} also works on glissandi."
}

{
  \override Score.SpacingSpanner.spacing-increment = 20
  \override Glissando.bound-details.left.text = \markup \italic gliss.
  \override Glissando.bound-details.left.padding = 1
  \override Glissando.bound-details.left.stencil-align-dir-y = #CENTER
  c'1\glissando
  c'''1
}
