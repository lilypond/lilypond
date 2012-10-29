\version "2.17.6"
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
  \phrasingSlurHalfDashed
  c\( d e  c\) |
  \phrasingSlurHalfSolid
  c\( d e  c\) |
  \phrasingSlurDashPattern #0.4 #2.0
  c\( d e  c\) |
  \once \override Slur.dash-definition = #'((0 0.25 1 1)
                                              (0.3 0.7 0.4 0.75)
                                              (0.75 1.0 1.0 1.0))
  c\( d e  c\) |
  \phrasingSlurSolid
  c\( d e  c\) |
}






