\score{
	\context PianoStaff <
	\context Staff=one \notes\relative c'{
		\stemup c4( c \translator Staff=two c )c |
		\translator Staff=one
		\stemup c4( c \translator Staff=two c )c |
		\stemup c4( c \translator Staff=one c )c |
		\translator Staff=two
		\stemup c4( c \translator Staff=one c )c |
		\translator Staff=two
		\stemup c4( \translator Staff=one c c )c |
		r2
		\translator Staff=two
		\stemup c4( \translator Staff=one c
		   \break
		c )c
		r2
%		\stemdown c4( \translator Staff=two c c \translator Staff=one )c
		\stemdown d4( \translator Staff=two c c \translator Staff=one )d
		\translator Staff=two
		\stemup c4( \translator Staff=one c c \translator Staff=two )c
		r1
	}
	\context Staff=two \notes\relative c'{
		\clef bass;
		s1 s1 s1 s1 s1 s1 s1 s1 s1 s1
	}
	>
	\paper{
		\translator{
			\PianoStaffContext
			minVerticalAlign = 3.0*\staffheight;
			maxVerticalAlign = 3.0*\staffheight;
		}
		%linewidth=100.\mm;
	}
}

\version "1.1.52"; 
