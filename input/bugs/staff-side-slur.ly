\score{
	\type Staff \notes\relative c''{
		% length matters...
%		\property Staff.instrument = "Tot"
		\property Staff.instrument = "ToetersToetersToeters"
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
		}
	}
}

