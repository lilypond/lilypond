
\version "1.3.120";

instrument = "Bassoon"

\include "header.ly"
\include "global.ly"
\include "fagotti.ly"

\score{
	\fagottiStaff
	\include "coriolan-part-paper.ly"
	\include "coriolan-midi.ly"
}

