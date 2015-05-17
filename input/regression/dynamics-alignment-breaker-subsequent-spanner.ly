\version "2.19.21"

\header {
  texidoc = "@code{\\breakDynamicSpan} shall only have an effect on the current
spanner, not on subsequent spanners.
"
}

\relative {
  % Check that the effect of \breakDynamic span is only for the current
  % spanner and not for the following spanners, too.
  c'1\<\breakDynamicSpan c''
  c,,1\>
  f,1\p % <= the \> and the \p should be aligned!
}
