\header {
    texidoc = "Polymetric music with aligned note values can be written
 by moving the timing engraver to staff context."
}

\score {

    \notes
    <
	\context Staff = SA
	{ % \time hardcodes \property Score.XXX = YYY
	\property Staff.timeSignatureFraction = #'(4 . 4)
	\property Staff.measureLength = #(make-moment 1 1)
	\property Staff.beatLength = #(make-moment 1 4)
	c1 c1 c1  }
	\context Staff= SB {
	\property Staff.timeSignatureFraction = #'(3 . 4)
	\property Staff.measureLength = #(make-moment 3 4)
	\property Staff.beatLength = #(make-moment 1 4)
	c2. c2.  c2. c2.  }

    >		 

    \paper{
    \translator{ \ScoreContext
    \remove "Timing_engraver" }
    \translator{ \StaffContext
    \consists "Timing_engraver"}
}
}
