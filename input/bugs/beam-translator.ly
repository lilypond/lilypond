\score{
	\type GrandStaff <
	\type Staff=one \notes\relative c'{
		s1
	}
	\type Staff=two \notes\relative c'{
		\clef bass;
		s2
		\translator Staff=one \stemup [c8 c c c ] 
		% the translator switch below, intended for the next beam,
		% affects (and breaks) the beam above
		\translator Staff=two
	}
	>
	\paper{
		% no slur damping
		slur_slope_damping = 10.0;
		\translator{
			\GrandStaffContext
			minVerticalAlign = 3.0*\staffheight;
			maxVerticalAlign = 3.0*\staffheight;
		}
		linewidth=-1.;
	}
}
