\version "2.19.21"

\header {
  texidoc = "Hairpins, DynamicTextSpanners and dynamics can be
positioned independently using @code{\\breakDynamicSpan}, which
causes the alignment spanner to end prematurely.
"
}

\relative {
  c'1^\<
  \dimTextDim
  c1_\>
  f,1\p

  c'1^\<
  \breakDynamicSpan
  c1_\>
  \breakDynamicSpan
  f,1\p
}
