\version "1.3.146"
\score{
	\context Staff \notes\relative c''{
		\property Staff.instrument = "Toeters"
		c c c c(\break
		)c c c c ~\break
		c c c c\break
	}
	\paper{
		linewidth=60.0\mm
%		\translator { \HaraKiriStaffContext }
		\translator { \OrchestralScoreContext }
		\translator {
			\StaffContext
			\consists "Instrument_name_engraver"
			marginHangOnClef = 1
		}
	}
}



