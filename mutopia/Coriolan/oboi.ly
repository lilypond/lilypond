\header{
filename =	 "oboi.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.9";

\include "oboe-1.ly"
\include "oboe-2.ly"

$oboi_staff = \type Staff = oboi <
	\property Staff.instrument = "oboe"
	\melodic< 
		\global;
		\$oboe1
		\$oboe2
	>
>

