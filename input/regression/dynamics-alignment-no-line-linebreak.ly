\version "2.19.21"

\header {
  texidoc = "Setting the style of a @code{DynamicTextSpanner} to @code{'none}
to hide the line altogether should also work over line breaks.
"
}

\relative {
  \override DynamicTextSpanner.style = #'none
  c''2\cresc g,2
  \break
  g2 c'2\f
}
