\header{
filename =	 "corni-part.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.0.3";

\include "global.ly"
\include "corni.ly"
\score{
	\$corni_staff
	\include "part-paper.ly"
	\midi{ \tempo 4 = 160; }
}

