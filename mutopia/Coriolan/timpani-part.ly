\header{
filename =	 "timpani-part.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.88";

\include "global.ly"
\include "timpani.ly"

\score{
	\timpaniStaff 
	\include "coriolan-part-paper.ly"
	\include "coriolan-midi.ly"
}

