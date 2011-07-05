\version "2.14.0"
\header {
  texidoc = "The left text of a @code{DynamicTextSpanner} is
left-aligned to its anchor note.
"
}

\relative c' {
  \crescTextCresc
  \dimTextDim
  c4\< c c c\!
  c4\> c c c\!
}
