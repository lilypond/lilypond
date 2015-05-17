\version "2.19.21"

\header {
  texidoc = "If the line for a @code{DynamicTextSpanner} is hidden, the
alignment spanner for dynamics is ended early.  This allows consecutive
dynamics to be unlinked."
}

\relative {
  g'4\p\cresc g g g
  g,1\f

  \override DynamicTextSpanner.style = #'none
  g'4\p\cresc g g g
  g,1\f  
}
