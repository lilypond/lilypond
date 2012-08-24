\version "2.16.0"

\header {
  texidoc = "The markup command @code{\\left-brace} selects a
@code{fetaBraces} glyph based on point size, using a binary search.
@code{\\right-brace} is simply a @code{\\left-brace} rotated 180
degrees.
"
}

\markup {
  \left-brace #30
  \hspace #2
  \right-brace #40
}
