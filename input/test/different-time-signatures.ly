\score{
    <
    	\context Staff=a \notes{
	    \time 3/4;
	    c4 c c | c c c |
	}
    	\context Staff=b \notes{
	    \time 2/4;
	    c4 c | c c | c c
	}
    >

    \paper{
    	linelength = -1.0\cm;
	\translator{
	    \ScoreContext
	    \remove Timing_engraver;
	}
	\translator{
	    \StaffContext
	    \consists Timing_engraver;
	}
    }
}
