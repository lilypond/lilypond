\version "2.3.4"

\header{ texidoc="@cindex Time Signature Multiple

@cindex polymetric music

You can have multiple time signatures occuring at the same time.

This is done by moving the timing engraver to staff context. Also,
@code{Staff} should be given the alias @code{Timing} to make @code{\\time}
command work correctly. The spacing is aligned vertically, although the bar 
lines seem to distort the regular spacing.

" }

%% Multiple time signatures seem not to be printed, however.
%% Should print bar lines according to the time signature of each staff. -HJJ

\score{
     \relative c'  <<
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
	\context{
	    \Score
	    \remove "Timing_engraver"
	}
	\context{
	    \Staff
	    \consists "Timing_engraver"
	    
	}
    }
}

