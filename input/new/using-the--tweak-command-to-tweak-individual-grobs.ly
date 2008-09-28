\version "2.11.61"
\header {
  lsrtags = "tweaks-and-overrides"
  texidoc = "
With the @code{\\tweak} command, every grob can be tuned directly.  Here
are some examples of available tweaks.
"
  doctitle = "Using the @code{\\tweak} command to tweak individual grobs"
}

\relative c' {
  \time 2/4
  \set fingeringOrientations = #'(right)
  <
    \tweak #'font-size #3 c
    \tweak #'color #red  d-\tweak #'font-size #8 -4
    \tweak #'style #'cross g
    \tweak #'duration-log #2 a
  >2
}
