\header{
filename = 	 "fagotti.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description = 	 "";
composer = 	 "Ludwig van Beethoven (1770-1827)";
enteredby = 	 "JCN";
copyright = 	 "public domain";
}

\version "1.3.120";

\include "fagotto-1.ly"
\include "fagotto-2.ly"

fagottiStaff =  \context VoiceCombineStaff = fagotti <
	\property VoiceCombineStaff.midiInstrument = #"bassoon"
	\property VoiceCombineStaff.instrument = #"2 Fagotti"
	\property VoiceCombineStaff.instr = #"Fg."
	%\clef "bass";
	% Ugh, clef broken in 1.3.125
	\property VoiceCombineStaff.clefGlyph = #"clefs-F"
	\property VoiceCombineStaff.clefPosition = #2
	\global
	\context VoiceCombineVoice=one \partcombine VoiceCombineVoice
		\context VoiceCombineThread=one \fagottoI
		\context VoiceCombineThread=two \fagottoII
>

