\version "1.3.146"

\header{
texidoc="
Slurs can be forced to always attach to note heads.
"
}

fragment = \notes {
  \property Voice.Slur \set #'direction = #1
  \property Voice.Slur \set #'attachment = #'(head . head)
  g''16()g()g()g()d'()d()d()d
}

\paper { linewidth = -1. } 

\score {
  \notes\relative c \fragment
  \paper { }  
}
