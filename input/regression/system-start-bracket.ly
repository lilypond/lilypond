\version "2.3.16"
\header {
    texidoc =
"
The piano brace should be shifted horizontally if it  is enclosed in a bracket.
"
}


\score {   {
    \context StaffGroup <<
	c4
	\context PianoStaff <<
	    \new Staff d
	    \new Staff e
	   >>
    >>
    }
    \paper {raggedright = ##t}
}

