
\version "1.3.120";

instrument = "Clarinet"

\include "header.ly"
\include "global.ly"
\include "clarinetti.ly"

\score{
	\clarinettiStaff
	\include "coriolan-part-paper.ly"
	\include "coriolan-midi.ly"
}

