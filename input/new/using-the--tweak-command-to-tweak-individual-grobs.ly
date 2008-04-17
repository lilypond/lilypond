\version "2.11.23"
\layout { ragged-right= ##t }
\header {
  lsrtags = "tweaks-and-overrides"
  texidoc = "
With the @code{\tweak} command, you can tune every grob directly. Here
are some examples of available tweaks.
"
  doctitle = "Using the @code{\tweak} command to tweak individual grobs"
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
