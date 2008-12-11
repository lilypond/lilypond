\version "2.11.65"
\header { texidoc = "
The appearance of phrasing slurs may be changed from solid to dotted or dashed.
"
}

\relative c'{
  c\( d e  c\) |
  \phrasingSlurDotted
  c\( d e  c\) |
  \phrasingSlurDashed
  c\( d e  c\) |
  \override PhrasingSlur #'dash-period = #2.0
  \override PhrasingSlur #'dash-fraction = #0.4
  c\( d e  c\) |
  \phrasingSlurSolid
  c\( d e  c\) |
}






