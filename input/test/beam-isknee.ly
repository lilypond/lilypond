\score{
	\context PianoStaff <
	\context Staff=one \notes\relative c'{
		s1
	}
	\context Staff=two \notes\relative c'{
		\clef bass;
% no knee
		\stemup [c8 \translator Staff=one \stemdown g'16 f]
		s8
		s2
	}
	>
	\paper{
 		linewidth=-1.;
	}
}

\version "1.3.59"; 
