\version "1.7.18"

\score{
	\context Staff <
		\context Voice=one \partcombine Voice
			\context Thread=one \notes\relative c''
				{
					d e f
				}
			\context Thread=two \notes\relative c''
				{
					d d d
				}
		>
}
%% new-chords-done %%
