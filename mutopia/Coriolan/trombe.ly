\header{
filename =	 "trombe.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.59";

\include "trombo-1.ly"
\include "trombo-2.ly"

trombeStaff = \context Staff = trombe <
	\property Staff.midiInstrument = #"trumpet"
	\property Staff.instrument = #"2 Trombe\n(C)"
	\property Staff.instr = #"Tbe.\n(C)"
	\time 4/4;
	\context Voice=one \partcombine Voice
		\context Thread=one \tromboI
		\context Thread=two \tromboII
>

