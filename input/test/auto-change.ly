
\score {
	\notes \context PianoStaff <
	\context Staff = "up" {
		\autochange Staff \relative c' { g4 a  b c d e f g }
	}
	\context Staff = "down" {
		\clef bass; 
		s1*2
	}

	>
	
}
