\version "1.7.16"
\header {
texidoc = "" 
}

\score{
	\notes\relative c''{
		\time 2/4
		\property Voice.Slur \set #'minimum-length = #40
		c()c
		c~c\break
		}
}
%% new-chords-done %%
