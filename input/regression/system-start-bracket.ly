\version "1.9.4"
\header {
    texidoc =
"
The piano brace should be shifted horizontally if it  is enclosed in a bracket.
"
}


\score { \notes  {
    \context StaffGroup <<
	c4
	\context PianoStaff <
	    d
	    e
	   >4
    >>
    }\paper {raggedright = ##t}}

