\header{
filename =	 "viola-part.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.0.19";

\include "global.ly"
\include "viola-1.ly"
\include "viola-2.ly"

$viola_staff = \context Staff = violai <
	\property Staff.midiInstrument = "viola"
	\property Staff.instrument = "Viola"
	\property Staff.instr = "Vla."
	\clef "alto";
	\notes \context Voice=one < 
		\global 
		\$viola1
		\$viola2
	>
>

\score{
	\$viola_staff
	\include "coriolan-part-paper.ly"
	\midi{ \tempo 4 = 160; }
}

