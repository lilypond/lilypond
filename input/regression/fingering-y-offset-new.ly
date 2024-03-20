\version "2.21.0"

\header {
  texidoc = "If a @code{Fingering} grob is left- or right-positioned, the
@code{Y-offset} property is taken relative to the vertical position of its
X-parent (i.e., its associated note head)."
}

{
  \set fingeringOrientations = #'(left)

  <c''-1>4
  \once \override Fingering.Y-offset = 2
  <c''-1>4
  \once \override Fingering.Y-offset = -2
  <c''-1>4
}
