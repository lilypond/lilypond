\header{
filename =	 "corni.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "1.3.42";

\include "corno-1.ly"
\include "corno-2.ly"

$corni_staff = \context Staff = corni <
	\property Staff.midiInstrument = #"french horn"
	\property Staff.instrument = #"2 Corni\n(E\\textflat)"
	\property Staff.instr = #"Cor.\n(E\\textflat)"
	% urg: can't; only My_midi_lexer:<non-static> () parses pitch?
	%\property Staff.transposing = "es"
	\property Staff.transposing = #3
	%\notes \context Voice=corni < 
	\notes \context Staff=corni < 
		\time 4/4;
		\skip 1*341; \bar "|.";
		\context VoiceOne=cornoi
			\$corno1
		\context VoiceTwo=cornoii
			\$corno2
	>
>

