\header{
filename =	 "corni.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.93";

\include "corno-1.ly"
\include "corno-2.ly"

corniStaff = \context VoiceCombineStaff = corni <
	\property VoiceCombineStaff.midiInstrument = #"french horn"
	\property VoiceCombineStaff.instrument = #"2 Corni\n(E\\textflat)"
	\property VoiceCombineStaff.instr = #"Cor.\n(E\\textflat)"
	% urg: can't; only My_midi_lexer:<non-static> () parses pitch?
	%\property VoiceCombineStaff.transposing = "es"
	\property VoiceCombineStaff.transposing = #3
	\time 4/4;
	\skip 1*314; \bar "|.";
	\context VoiceCombineVoice=one \partcombine VoiceCombineVoice
		\context VoiceCombineThread=one \cornoI
		\context VoiceCombineThread=two \cornoII
>

