\version "1.3.146"


fragment = \notes {
  \property Voice.Beam \set #'staff-position = #4
  \property Voice.Beam \set #'height = #-4
  [c'8 c]
}

\paper { linewidth = -1. } 

\score {
  \notes\relative c \fragment
  \paper { }  
}
