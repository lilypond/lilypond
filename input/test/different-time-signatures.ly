
% barline spacing  disrupts visual rhythm.

\scm "(set! space-alist (assoc-set! space-alist '(\"Staff_bar\" \"begin-of-note\") '(minimum_space 0.0)))";
\scm "(set! space-alist (assoc-set! space-alist '(\"\" \"Staff_bar\") '(minimum_space 0.0)))";

\score{
    \notes \relative c'  <
    	\context Staff= AS {
	    \time 3/4;
	    c4 c c | c c c |
	}
    	\context Staff=BS {
	    \time 2/4;
	    c4 c | c c | c c
	}
	% TODO: make c4. here align  with c4 there.
    	\context Staff =CS {
	    \time 3/8;
	    c4. c8 c c   c4. c8 c c
	}
    >

    \paper{
    	linewidth = -1.0\cm;
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
