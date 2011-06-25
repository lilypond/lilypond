\version "2.14.2"

\header {
  texidoc = "Text spanners ending on full-measure rests do
not stop prematurely on preceding note heads."
}

\relative c'' {
  a1\startTextSpan
  b1
  R1\stopTextSpan
}
