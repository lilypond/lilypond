\version "1.7.18"
\header{
	texidoc="@cindex Beam Neutral Direction
When a beam falls in the middle of the staff, Lilypond normally
prints the beam pointing down.  However, this behaviour can be
altered if desired.
" }

fragment = \notes {
   b''8-[ b]
  \property Voice.Beam \set #'neutral-direction = #-1
   b-[ b]
  \property Voice.Beam \set #'neutral-direction = #1
   b-[ b]
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper { }  
}
%% new-chords-done %%
