\version "1.5.68"
\header {
    texidoc =
"
The piano brace should be shifted horizontally if it  is enclosed in a bracket.
"
}


\score { \notes  {
    \context StaffGroup <
	c4
	\context PianoStaff <
	    d4
	    e4
	   >
    >
    }\paper {linewidth = -1. }}
