\version "1.7.6"
\header {
    texidoc =
"
The piano brace should be shifted horizontally if it  is enclosed in a bracket.
"
}


\score { \notes  {
    \context StaffGroup <
	c4
	\context PianoStaff <<
	    d
	    e
	   >>4
    >
    }\paper {linewidth = -1. }}
%% new-chords-done %%
