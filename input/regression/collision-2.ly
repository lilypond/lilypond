\version "1.5.68"

\score {
    \notes

  \context Staff \notes <  
      %% the f and g on 4th beat are exceptionally ugh.
      \context Voice=i { \stemUp c4 d e f g2 g4 a | }
      \context Voice=ii { \stemDown g4 f e g  g2 g2 } 
  >
}

