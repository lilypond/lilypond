\header {
    texidoc = "Polymetric music with aligned note values can be written
 by moving the timing engraver to staff context."
}

\score {

    \notes
    <
	\context Staff = SA
	{
	    \time 4/4
	c1 c1 c1
	\bar "|."
    }
    \context Staff= SB {
	\time 3/4
	c2. c2.  c2. c2.
	\bar "|."
    }

    >		 

    \paper{
    \translator{ \ScoreContext
    \remove "Timing_engraver" }
    \translator{ \StaffContext
    \consists "Timing_engraver"
    \alias Timing
}
}
}
