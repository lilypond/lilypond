\version "1.5.68"


\score {
	\notes	\context StaffGroup < \context Staff = SA { s1 }
		\context Staff = SB { s1 }>
}
