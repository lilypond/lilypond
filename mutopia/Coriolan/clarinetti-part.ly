\header{
filename =	 "clarinetti-part.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "0.1.14";

\include "global.ly"
\include "clarinetti.ly"
\include "part-paper.ly"
\score{
	\$clarinetti_staff
	\paper{}
}

