\header{
filename =	 "flauti.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "1.3.59";

\include "flauto-1.ly"
\include "flauto-2.ly"

$flauti_staff = \notes \context Staff = flauti <
	\property Staff.midiInstrument = #"flute"
	\property Staff.instrument = #"2 Flauti"
	\property Staff.instr = #"Fl."

	\global

	\context Voice=one { \skip 1; }
	\context Voice=two { \skip 1; }

	\context Voice=one \partcombine Voice <
		\context Thread=one \$flauto1
		\context Thread=two \$flauto2
	>
>

