\version "1.7.16"
\header {
texidoc = "" 
}

\score{
  \context TabStaff <
	\notes\relative c''{
		c()d
		d()d
		d()c
  }
  >
}
%% new-chords-done %%
