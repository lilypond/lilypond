\header{
filename =	 "oboi.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "1.3.4";

\include "oboe-1.ly"
\include "oboe-2.ly"

$oboi_staff = \context Staff = oboi <
	\property Staff.midiInstrument = "oboe"
	\property Staff.instrument = "2 Oboi"
	\property Staff.instr = "Ob."
	\notes \context Voice=oboi < 
		\global
		\$oboe1
		\$oboe2
	>
>

