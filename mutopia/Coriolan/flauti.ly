\header{
filename =	 "flauti.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.93";

\include "flauto-1.ly"
\include "flauto-2.ly"

flautiStaff = \notes \context VoiceCombineStaff = flauti <
	\property VoiceCombineStaff.midiInstrument = #"flute"
	\property VoiceCombineStaff.instrument = #"2 Flauti"
	\property VoiceCombineStaff.instr = #"Fl."
	\global
	\context VoiceCombineVoice=one \partcombine VoiceCombineVoice
		\context VoiceCombineThread=one \flautoI
		\context VoiceCombineThread=two \flautoII
>

