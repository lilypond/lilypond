%{

 Brahms Klavierst"ucke op 119, no 1.

%}

\score {
    \notes\context PianoStaff <
    \context Staff = up
    \relative c'' <
	{ fis4-3 ( ~ fis16 )a-5 } \\
	{
	    \dynamicUp
	    fis16( \> d \! b \translator Staff = down \clef treble g ~ < g8 )e> } \\
	{ s16
	  \property Voice.Stem \set #'transparent = ##t
	  d'
	  \property Voice.Stem \revert #'transparent
	  ~ < d4 b4  > }
    >
    \context Staff = down { \time 3/8 \clef bass s4. }
    >
    \paper { linewidth = -1. }
}

