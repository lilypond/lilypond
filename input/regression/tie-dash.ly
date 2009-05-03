\version "2.13.1"
\header {texidoc = "@cindex Tie, dotted, dashed
The appearance of ties may be changed from solid to dotted or dashed.
"
} 
\layout{ ragged-right=##t }


\relative c'{
  c2 ~ c |
  \tieDotted
  c2 ~ c |
  \tieDashed
  c2 ~ c |
  \tieHalfDashed
  c2 ~ c |
  \tieHalfSolid
  c2 ~ c |
  \tieDashPattern #0.4 #2.0
  c2 ~ c |
  \once \override Tie #'dash-definition = #'((0 0.25 1 1)
                                             (0.3 0.7 0.4 0.75)
                                             (0.75 1.0 1.0 1.0))
  c2 ~ c |
  \tieSolid
  c2 ~ c |
}






