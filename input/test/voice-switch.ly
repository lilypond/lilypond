\version "1.7.18"
% delete.  We have other partcombine examples -- if that's
% what this is.  -gp

\score{
	\context Staff <
		\context Voice=one\skip 1
		\context Voice=two\skip 1
		\context Voice=one \partcombine Voice
			\context Thread=one \notes\relative c'' {
				c2 c2
				c2 c2
			}
			\context Thread=two \notes\relative c'' {
				b2 a4 (a-)
				a2 a4 (a-)
				
			}
	>
	\paper{
		linewidth=140.\mm
	}
}


%% new-chords-done %%
