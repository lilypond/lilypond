\version "1.7.6"


fragment = \notes {
  [b''8 b]
  \property Voice.Beam \set #'neutral-direction = #-1
  [b b]
}

\paper { linewidth = -1. } 

\score {
  \notes\relative c \fragment
  \paper { }  
}
%% new-chords-done %%
