\header{
filename =	 "oboi.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "1.3.59";

\include "oboe-1.ly"
\include "oboe-2.ly"

$oboi_staff = \context Staff = oboi <
	\property Staff.midiInstrument = #"oboe"
	\property Staff.instrument = #"2 Oboi"
	\property Staff.instr = #"Ob."
	%\notes \context Voice=oboi < 
	\notes \context Staff=oboi < 
		\global
		\context VoiceOne=oboei
			\$oboe1
		\context VoiceTwo=oboeii
			\$oboe2
	>
>

