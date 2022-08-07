\version "2.23.12"

\header {
  texidoc = "The direction of a trill spanner can be set with @code{_}
and @code{^} indicators."
}

{
  \voiceTwo % sets DOWN by default, but ^ and _ should have precedence
  g\startTrillSpan g^\startTrillSpan g_\startTrillSpan g-\startTrillSpan
}
