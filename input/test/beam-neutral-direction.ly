

fragment = \notes {
  [b''8 b]
  \property Voice.Beam \set #'default-neutral-direction = #-1
  [b b]
}

\paper { linewidth = -1.; } 

\score {
  \notes\relative c \fragment
  \paper { }  
}
