\version "1.7.18"
%  this is the same thing as beam-control.ly, but it's a
%  worse example.  Definately delete it.
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
