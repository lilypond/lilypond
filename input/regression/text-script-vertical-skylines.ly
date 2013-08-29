\version "2.17.21"

\header {
  texidoc = "@code{TextScript}s are spaced closely, following outlines of
the stencil.  When markup commands like @code{pad-around} and
@code{with-dimensions} change the extent of a stencil, these changed
extents have effect in the stencil outline used to place the resulting
@code{TextScript}."
}

{
  g'2^\markup { g \line {. . . .} }
  e'^\markup { e }

  g'2^\markup { g \transparent \line {. . . .} }
  e'^\markup { e }

  g'2^\markup { g \pad-around #0.5 \line{. . . .} }
  e'^\markup { e }

  g'2^\markup { g \with-dimensions #'(-0 . 0) #'(-0 . 0) \line {. . . .} }
  e'^\markup { e }
}
