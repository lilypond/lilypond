\header{
filename =	 "flauti.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "1.0.7";

\include "flauto-1.ly"
\include "flauto-2.ly"

$flauti_staff = \type Staff = flauti <
	\property Staff.midi_instrument = "flute"
	\property Staff.instrument = "2 Flauti"
	\property Staff.instr = "Fl."
	\notes \type Voice=flauti < 
		\global
		\$flauto1
		\$flauto2
	>
>

