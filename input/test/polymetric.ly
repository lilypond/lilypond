\version "1.7.18"

\header{ texidoc="@cindex Time Signature Multiple

@cindex polymetric music

You can have multiple time signatures occuring at the same time.

This is done by moving the timing engraver to staff context. Also,
Staff should be given the alias @code{Timing} to make @code{\time}
command work correctly. Barlines distort the regular spacing, though.

If the note durations do not line up, \times can be used to scale up a
staff. The second staff has a 3/4 signature with a 3/5 tuplet over all
of the notes. Switching off the tuplet bracket, and manually printing
a 10/8 sign gives the desired effect.

" }

\score{
    \notes \relative c'  <
    	\context Staff= AS {
	    \time 3/4
	    c4 c c | c c c |
	}
    	\context Staff= DS {
	    \time 3/4
	    \property Staff.timeSignatureFraction= #'(10 . 8)
	    \property Staff.tupletInvisible = ##t
	    \times 3/5 { c4. c4. c4 c4 |  c4. c4. c4 c4  }
	}
    	\context Staff=BS {
	    \time 2/4
	    c4 c | c c | c c
	}
    	\context Staff =CS {
	    \time 3/8
	    c4. c8 c c   c4. c8 c c
	}
    >

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

