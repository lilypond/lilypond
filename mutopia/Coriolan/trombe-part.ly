\header{
filename =	 "trombe-part.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.59";

\include "global.ly"
\include "trombe.ly"
\score{
	\trombeStaff 
	\include "coriolan-part-combine-paper.ly"
	\include "coriolan-midi.ly"
}

