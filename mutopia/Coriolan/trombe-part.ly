
\version "1.3.120";

\include "header.ly"
\include "global.ly"
\include "trombe.ly"

\score{
	\trombeStaff 
	\include "coriolan-part-combine-paper.ly"
	\include "coriolan-midi.ly"
}

