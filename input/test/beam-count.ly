\version "1.7.16"


fragment = \notes {
  f'32 g a b b a g f

  \property Voice.autoBeamSettings
    \set #'(end * * * *) = #(ly:make-moment 1 4)
  f32 g a b b a g f

  f32 g a
  \property Voice.stemRightBeamCount = #1 b
  \property Voice.stemLeftBeamCount = #1 b
  a g f
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper { }  
}
%% new-chords-done %%
