\header{
filename =	 "clarinetti.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.14";

\include "clarinetto-1.ly"
\include "clarinetto-2.ly"

$clarinetti_staff = \type Staff = clarinetti <
	\property Staff.instrument = "clarinet"
	\melodic< 
% ugh, key doesn't transpose along
%		\global
		\meter 4/4;
		\key bes;
		\transpose d \$clarinetto1
		\transpose d \$clarinetto2
	>
>

