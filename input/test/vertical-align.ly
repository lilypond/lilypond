\version "1.7.18"
% regression or delete.  -gp

\score {
\notes <
	\context GrandStaff < {
	   c'1 \break  c'''''1 
	}
	    { c'1 \break c,,,,1}
	>

>

\paper{
  \translator{\StaffContext
    VerticalAlignment \override #'threshold = #'(7 . 12)
  }
}

}
%% new-chords-done %%
