
\score {
	\notes \context PianoStaff <
		\context Staff = "up" {
			\autochange Staff \relative c' { 
				[c8 e'] [c' c,,]
				\stemdown
				c'8 c c g,
				g8 d' d d 
				\stemup
				b8 c d e
			}
		}
		\context Staff = "down" {
			\clef bass; 
			s1*2
		}
	>
	\paper{
		\translator{
			\StaffContext
			autoKneeGap = "13.0";
			autoInterstaffKneeGap = "4.0";
		}
	}
}
