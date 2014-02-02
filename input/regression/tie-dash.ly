\version "2.19.2"
\header {texidoc = "@cindex Tie, dotted, dashed
The appearance of ties may be changed from solid to dotted or dashed.
"
} 
\layout{ ragged-right=##t }


\relative c'{
  c2 ~ 2 |
  \tieDotted
  c2 ~ 2 |
  \tieDashed
  c2 ~ 2 |
  \tieHalfDashed
  c2 ~ 2 |
  \tieHalfSolid
  c2 ~ 2 |
  \tieDashPattern #0.4 #2.0
  c2 ~ 2 |
  \once \override Tie.dash-definition = #'((0 0.25 1 1)
                                             (0.3 0.7 0.4 0.75)
                                             (0.75 1.0 1.0 1.0))
  c2 ~ 2 |
  \tieSolid
  c2 ~ 2 |
}






