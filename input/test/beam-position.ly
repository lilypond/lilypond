\version "1.7.18"


fragment = \notes {
  \property Voice.Beam \set #'positions = #'(4 . 0)
   c'8-[ c]
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper { }  
}
%% new-chords-done %%
