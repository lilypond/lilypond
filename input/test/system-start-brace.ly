\version "1.3.146"


\score {
	\notes	\context PianoStaff < \context Staff = SA { s1 }
		\context Staff = SB { s1 }>
}
