
fragment = \notes {
  \property Voice.Beam \set #'direction = #1
  \property Voice.Beam \set #'height-hs = #0
  [a''8 e' d c]
}

\paper { linewidth = -1.; } 

\score {
  \notes\relative c \fragment
  \paper { }  
}
