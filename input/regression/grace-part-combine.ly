#(ly:set-option 'old-relative)
\version "1.9.0"
\header {
    texidoc = "Partcombiner and grace notes can go together."
}
    \paper { raggedright= ##t }

\score {
    \context StaffGroup = group <
	\context Staff = instrument <
	    \context Voice=one \partcombine Voice
	    \context Thread=one \notes \relative c'' {
		c4 d e f  \grace f16 g1
	    }
	    \context Thread=two \notes \relative c' {
		c4 d e2  g1
	    }
	>
    >
}

