\header{
filename =	 "clarinetti-part.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.110";

\include "global.ly"
\include "clarinetti.ly"
\score{
	\clarinettiStaff
	\include "coriolan-part-combine-paper.ly"
	\include "coriolan-midi.ly"
}

