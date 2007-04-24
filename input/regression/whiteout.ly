\header {

  texidoc = "The whiteout command underlays a white box under a
markup.  The whitening effect only is only guaranteed for staff lines,
since staff lines are in a lower layer than most other grobs.  "

}
\version "2.10.0"

\paper
{
  ragged-right = ##t
}

{
  \override TextScript #'extra-offset = #'(2 . 4)
  c'4-\markup  { \whiteout \pad-markup #0.5 foo } c
} 
