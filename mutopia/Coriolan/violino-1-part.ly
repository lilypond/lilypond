
\version "1.3.120";

instrument = "Violin I"

\include "header.ly"
\include "global.ly"
\include "violino-1.ly"

\score{
	\violinoIStaff 
	\include "coriolan-part-paper.ly"
	\include "coriolan-midi.ly"
}

