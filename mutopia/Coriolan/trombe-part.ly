
\version "1.3.120";

instrument = "Trumpet"

\include "header.ly"
\include "global.ly"
\include "trombe.ly"

\score{
	\trombeStaff 
	\include "coriolan-part-paper.ly"
	\include "coriolan-midi.ly"
}

