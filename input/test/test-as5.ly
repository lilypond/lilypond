\include "paper-as5.ly"

\score {
	\notes\relative c''{
		\time 4/4;
		g1 a b c 
		g2 a b c
		g4 a b c
	}
	\paper {
		\paper_as_five
    		\translator { \StaffContext barSize = #5 }
	}

}

