\version "1.7.18"
\header{texidoc = "REGRESSION or DELETE. "}

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
