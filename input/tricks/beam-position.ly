

fragment = \notes {
  \property Voice.Beam \set #'staff-position = #2
  \property Voice.Beam \set #'height = #-2
  [c'8 c]
}

\paper { linewidth = -1.; } 

\score {
  \notes\relative c \fragment
  \paper { }  
}
