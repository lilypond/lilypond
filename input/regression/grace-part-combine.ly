\header {
    texidoc = "partcombiner and grace notes can go together"
}

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
