\version "1.7.2"


fragment = \notes {
  a'''' b c a
  \property Voice.TextSpanner \set #'type = #'dotted-line
  \property Voice.TextSpanner \set #'edge-height = #'(0 . 1.5)
  \property Voice.TextSpanner \set #'edge-text = #'("8va " . "")
  \property Staff.centralCPosition = #-13
  a#(ly-export (make-span-event 'TextSpanEvent START)) b c a #(ly-export (make-span-event 'TextSpanEvent STOP))
}

\paper { linewidth = -1. } 

\score {
  \notes\relative c \fragment
  \paper { }  
}
