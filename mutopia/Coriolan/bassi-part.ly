\version "1.3.120";

instrument = "Violoncello and Contrabass"

\include "header.ly"
\include "global.ly"
\include "bassi.ly"

\score{
	\bassiGroup
	\include "coriolan-part-paper.ly"
	\include "coriolan-midi.ly"
}

