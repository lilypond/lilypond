\score{
	\context Staff \notes\relative c''{
		\property Staff.instrument = "Toeters"
		c c c c(\break
		)c c c c ~\break
		c c c c\break
	}
	\paper{
		linewidth=60.0\mm;
%		\translator { \HaraKiriStaffContext }
		\translator { \OrchestralScoreContext }
		\translator {
			\StaffContext
			\consists "Staff_margin_engraver";
			marginHangOnClef = 1;
		}
	}
}


\version "1.0.21"; 
