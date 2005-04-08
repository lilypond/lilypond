\header {

  texidoc = "The whiteout command underlays a white box under a
markup.  The whitening effect only is only guaranteed for staff lines,
since staff lines are in a different layer.  "

}
\version "2.5.18"

\paper
{
  raggedright = ##t
}

{
  \override TextScript #'extra-offset = #'(2 . 4)
  c'4-\markup  { \whiteout foo } c
} 
