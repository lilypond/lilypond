
\version "1.3.120";

instrument = "Timpani"

\include "header.ly"
\include "global.ly"
\include "timpani.ly"

\score{
	\timpaniStaff 
	\include "coriolan-part-paper.ly"
	\include "coriolan-midi.ly"
}

