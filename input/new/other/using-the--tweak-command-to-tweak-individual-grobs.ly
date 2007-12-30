\version "2.11.23"

\header { texidoc = "
With the @code{\tweak} command, you can tune every grob directly. Here
are some examples of available tweaks.


" }

\paper {
  ragged-right = ##t
}

{
  \set fingeringOrientations = #'(right)
  <
    \tweak #'font-size #3  c
    \tweak #'color #red  d-\tweak #'font-size #8 -4
    \tweak #'style #'cross  g
    \tweak #'duration-log #1  a
  >4
}
