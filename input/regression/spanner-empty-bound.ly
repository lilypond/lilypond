\version "2.21.2"

\header {
  texidoc = "Empty bounds on a line spanner do not cause LilyPond to get stuck
  in an infinite loop."
}

{
  1-\tweak bound-details.left.text "" \startTrillSpan
  1\stopTrillSpan
}
