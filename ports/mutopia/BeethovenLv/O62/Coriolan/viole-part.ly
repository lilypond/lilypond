
\version "1.3.141";

instrument = "Viola"

\include "header.ly"
\include "global.ly"
\include "viole.ly"

\score{
	\violeGroup
	\include "coriolan-part-paper.ly"
	\include "coriolan-midi.ly"
}

