\version "2.16.0"

\header {
  texidoc = "Setting the style of a @code{DynamicTextSpanner} to @code{'none}
to hide the line altogether should also work over line breaks.
"
}

\relative c'' {
  \override DynamicTextSpanner #'style = #'none
  c2\cresc g,2
  \break
  g2 c'2\f
}
