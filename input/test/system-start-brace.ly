\version "1.5.68"


\score {
	\notes	\context PianoStaff < \context Staff = SA { s1 }
		\context Staff = SB { s1 }>
}
