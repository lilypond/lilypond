\version "2.1.26"
\header {
    texidoc =
"
The piano brace should be shifted horizontally if it  is enclosed in a bracket.
"
}


\score { \notes  {
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

