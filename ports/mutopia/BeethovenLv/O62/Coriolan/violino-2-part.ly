
\version "1.3.141";

instrument = "Violin II"

\include "header.ly"
\include "global.ly"
\include "violino-2.ly"

\score{
	\violinoIIStaff 
	\include "coriolan-part-paper.ly"
	\include "coriolan-midi.ly"
}

