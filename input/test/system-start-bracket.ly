\version "1.7.18"
% delete.

\score {
	\notes	\context StaffGroup < \context Staff = SA { s1 }
		\context Staff = SB { s1 }>
}
%% new-chords-done %%
