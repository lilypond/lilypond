\version "1.3.146"


% barline spacing  disrupts visual rhythm.

#(define nbal (acons '("Staff_bar" "begin-of-note") '(minimum_space 0.0) default-break-align-space-alist))

\score{
    \notes \relative c'  <
    	\context Staff= AS {
	    \property Staff.timeSignatureFraction = #'(3 . 4) 
	    c4 c c | c c c |
	}
    	\context Staff=BS {
	    \property Staff.timeSignatureFraction = #'(2  . 4) 	    
	    c4 c | c c | c c
	}
	% TODO: make c4. here align  with c4 there.
    	\context Staff =CS {
	    \property Staff.timeSignatureFraction = #'(3 . 8) 
	    c4. c8 c c   c4. c8 c c
	}
    >

    \paper{
    	linewidth = -1.0\cm
	\translator{
	    \ScoreContext
	    BreakAlignment \override #'space-alist = #nbal
	    \remove Timing_engraver
	}
	\translator{
	    \StaffContext
	    \consists Timing_engraver
	}
    }
}
