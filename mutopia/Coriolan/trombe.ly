\header{
filename =	 "trombe.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.14";

\include "trombo-1.ly"
\include "trombo-2.ly"

$trombe_staff = \type Staff = trombe <
	\melodic< 
%		\global;
		\meter 4/4;
		\$trombo1
		\$trombo2
	>
>

