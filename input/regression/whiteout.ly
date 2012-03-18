\header {

  texidoc = "The whiteout command underlays a white box under a
markup.  "

}
\version "2.14.0"

\paper
{
  ragged-right = ##t
}

\relative c'' {
  \override TextScript #'layer = #'2
  \override TextScript #'extra-offset = #'(2 . 4)
  c4-\markup  { \whiteout \pad-markup #0.5 foo } c
}
