\header{
filename =	 "trombe.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "1.0.21";

\include "trombo-1.ly"
\include "trombo-2.ly"

$trombe_staff = \context Staff = trombe <
	\property Staff.midiInstrument = "trumpet"
	\property Staff.instrument = "2 Trombe (C)"
	\property Staff.instr = "Tbe."
	\notes \context Voice=trombe < 
%		\global
		\time 4/4;
		\$trombo1
		\$trombo2
	>
>

