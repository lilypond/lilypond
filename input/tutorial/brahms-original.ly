\score {
    \notes\context PianoStaff <
    \context Staff = up
    \relative c'' <
	{   \key d\major 
	    fis4-3_\p ( ~
	    fis16 )a-5 } \\
	{
	    fis16( \> d \! b \translator Staff = down
	    \clef treble g ~ < g8 )e> } \\
	{ s16
	  d'
	  ~ < d4 b4  > }
    >
    \context Staff = down {
	\key d \major
	\time 3/8 \clef bass s4. }
    >
    \paper { linewidth = -1. }
}

