
\version "1.3.141";

instrument = "Oboe"

\include "header.ly"
\include "global.ly"
\include "oboi.ly"

\score{
	\oboiStaff 
	\include "coriolan-part-paper.ly"
	\include "coriolan-midi.ly"
}

