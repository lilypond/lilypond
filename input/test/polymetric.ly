\version "2.1.23"

\header{ texidoc="@cindex Time Signature Multiple

@cindex polymetric music

You can have multiple time signatures occuring at the same time.

This is done by moving the timing engraver to staff context. Also,
Staff should be given the alias @code{Timing} to make @code{\\time}
command work correctly. Barlines distort the regular spacing, though.


" }

\score{
    \notes \relative c'  <<
    	\new Staff {
	    \time 3/4
	    c4 c c | c c c |
	}

    	\new Staff {
	    \time 2/4
	    c4 c | c c | c c
	}
    	\new Staff {
	    \time 3/8
	    c4. c8 c c   c4. c8 c c
	}
    >>

    \paper{
    	raggedright = ##t
	\translator{
	    \ScoreContext
	    \remove "Timing_engraver"
	}
	\translator{
	    \StaffContext
	    \consists "Timing_engraver"
	    \alias "Timing"
	}
    }
}

