\include "paper-as5.ly"

\score {
	\notes\relative c''{
		\time 4/4;
		g1 g2 g4. g8 |
	}
	\paper {
		\paper_as_five
    		\translator { \StaffContext barSize = #5 }
	}

}

