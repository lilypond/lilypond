\version "1.7.18"
\header{
	texidoc="@cindex Beam Count
You can alter the number of stems in a beam.  Here we see
two sets of four 32nds joined as if they were 8th notes.
" }

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
  \paper { raggedright = ##t}  
}
%% new-chords-done %%
