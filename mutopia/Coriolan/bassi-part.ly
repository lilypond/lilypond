\header{
filename = 	 "bassi-part.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description = 	 "";
composer = 	 "Ludwig van Beethoven (1770-1827)";
enteredby = 	 "JCN";
copyright = 	 "public domain";
}

\version "1.3.120";

\include "bassi.ly"

\score{
	\bassiGroup
	\include "coriolan-part-combine-paper.ly"
	\include "coriolan-midi.ly"
}

