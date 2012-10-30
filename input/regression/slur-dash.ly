\version "2.17.6"
\header {texidoc = "@cindex Slur, dotted, dashed
The appearance of slurs may be changed from solid to dotted or dashed.
"
} 
\layout{ ragged-right=##t }


\relative c'{
  c( d e c) |
  \slurDotted
  c( d e c) |
  \slurDashed
  c( d e c) |
  \slurHalfDashed
  c( d e c) |
  \slurHalfSolid
  c( d e c) |
  \slurDashPattern #0.4 #2.0
  c( d e c) |
  \once \override Slur.dash-definition = #'((0 0.25 1 1)
                                              (0.3 0.7 0.4 0.75)
                                              (0.75 1.0 1.0 1.0))
  c( d e c) |
  \slurSolid
  c( d e c) |
}






