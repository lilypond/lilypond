
\version "1.3.120";

instrument = "French Horn"

\include "header.ly"
\include "global.ly"
\include "corni.ly"

\score{
	\corniStaff
	\include "coriolan-part-paper.ly"
	\include "coriolan-midi.ly"
}


