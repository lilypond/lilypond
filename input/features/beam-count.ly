
fragment = \notes {
  f'32 g a b b a g f

  \property Voice.autoBeamSettings
    \set #'(end * * * *) = #(make-moment 1 4)
  f32 g a b b a g f

  f32 g a
  \property Voice.stemRightBeamCount = #1 b
  \property Voice.stemLeftBeamCount = #1 b
  a g f
}

\paper { linewidth = -1.; } 

\score {
  \notes\relative c \fragment
  \paper { }  
}
