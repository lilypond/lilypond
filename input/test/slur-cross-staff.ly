\score{
	\context PianoStaff <
	\context Staff=one \notes\relative c'{
		\stemUp c4( c \translator Staff=two c )c |
		\translator Staff=one
		\stemUp c4( c \translator Staff=two c )c |
		\stemUp c4( c \translator Staff=one c )c |
		\translator Staff=two
		\stemUp c4( c \translator Staff=one c )c |
		\translator Staff=two
		\stemUp c4( \translator Staff=one c c )c |
		r2
		\translator Staff=two
		\stemUp c4( \translator Staff=one c
		   \break
		c )c
		r2
%		\stemDown c4( \translator Staff=two c c \translator Staff=one )c
		\stemDown d4( \translator Staff=two c c \translator Staff=one )d
		\translator Staff=two
		\stemUp c4( \translator Staff=one c c \translator Staff=two )c
		r1
	}
	\context Staff=two \notes\relative c'{
		\clef bass;
		s1 s1 s1 s1 s1 s1 s1 s1 s1 s1
	}
	>
}

\version "1.3.96"; 
