\version "1.7.18"


fragment = \notes {
   b''8-[ b]
  \property Voice.Beam \set #'neutral-direction = #-1
   b-[ b]
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper { }  
}
%% new-chords-done %%
