\header{
filename = 	 "trombe.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description = 	 "";
composer = 	 "Ludwig van Beethoven (1770-1827)";
enteredby = 	 "JCN";
copyright = 	 "public domain";
}

\version "1.3.120";

\include "trombo-1.ly"
\include "trombo-2.ly"

trombeStaff =  \context VoiceCombineStaff = trombe <
	\context VoiceCombineStaff=trombe {
		\property VoiceCombineStaff.midiInstrument = #"trumpet"
		\property VoiceCombineStaff.instrument = #"2 Trombe\n(C)"
		\property VoiceCombineStaff.instr = #"Tbe.\n(C)"
		\skip 1*314; 
		\bar "|."; 
	}
	\context VoiceCombineVoice=one \partcombine VoiceCombineVoice
		\context VoiceCombineThread=one \tromboI
		\context VoiceCombineThread=two \tromboII
>

