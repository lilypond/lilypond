\header{
filename =	 "oboi.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "1.0.7";

\include "oboe-1.ly"
\include "oboe-2.ly"

$oboi_staff = \type Staff = oboi <
	\property Staff.midi_instrument = "oboe"
	\property Staff.instrument = "2 Oboi"
	\property Staff.instr = "Ob."
	\notes \type Voice=oboi < 
		\global
		\$oboe1
		\$oboe2
	>
>

