\header{
filename =	 "flauti.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.10";

\include "flauto-1.ly"
\include "flauto-2.ly"

$flauti_staff = \type Staff = flauti <
	\property Staff.instrument = "flute"
	\melodic< 
		\global;
		\$flauto1
		\$flauto2
	>
>

