\score{
	\context PianoStaff <
	\context Staff=one \notes\relative c'{
		\stemup [c8 c \translator Staff=two \stemup c c]
		[c c c c]
		\translator Staff=one
		\stemdown [c8 c \translator Staff=two \stemup c c]
		r2
		\stemdown [c8 c \translator Staff=one \stemdown c c]
		r2
		\translator Staff=two
		\stemup [c8 c \translator Staff=one \stemdown c c]
		r2
	}
	\context Staff=two \notes\relative c'{
		\clef bass;
		s1
		s1
		s1
		s1
	}
	>
}

\version "1.3.59"; 
