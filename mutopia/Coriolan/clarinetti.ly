\header{
filename =	 "clarinetti.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.88";

\include "clarinetto-1.ly"
\include "clarinetto-2.ly"

clarinettiStaff = \context VoiceCombineStaff = clarinetti <
	\property VoiceCombineStaff.midiInstrument = #"clarinet"
	\property VoiceCombineStaff.instrument = #"2 Clarinetti\n(B\\textflat)"
	\property VoiceCombineStaff.instr = #"Cl.\n(B\\textflat)"
	% urg: can't; only My_midi_lexer:<non-static> () parses pitch?
	%\property VoiceCombineStaff.transposing = "bes"
	\property VoiceCombineStaff.transposing = #-2
	\time 4/4;
	\notes \key f \major;
	\skip 1*314; \bar "|.";
	\context VoiceCombineVoice=one \partcombine VoiceCombineVoice
		\context VoiceCombineThread=one \clarinettoI
		\context VoiceCombineThread=two  \clarinettoII
>

