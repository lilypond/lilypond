
fragment = \notes {
  \property Voice.Beam \set #'y-position-hs = #4
  \property Voice.Beam \set #'height-hs = #-4
  [c'8 c]
}

\paper { linewidth = -1.; } 

\score {
  \notes\relative c \fragment
  \paper { }  
}
