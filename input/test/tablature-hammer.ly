\version "1.7.6"
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
