\version "1.7.18"
\header {texidoc = "DELETE. "} 

\score {
	\notes	\context PianoStaff < \context Staff = SA { s1 }
		\context Staff = SB { s1 }>
}
%% new-chords-done %%
