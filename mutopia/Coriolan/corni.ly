\header{
filename =	 "corni.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "1.0.7";

\include "corno-1.ly"
\include "corno-2.ly"

$corni_staff = \type Staff = corni <
	\property Staff.midi_instrument = "french horn"
	\property Staff.instrument = "2 Corni (E\\textflat)"
	\property Staff.instr = "Cor. (E\\textflat)"
	% urg: can't; only My_midi_lexer:<non-static> () parses pitch?
	%\property Staff.transposing = "es"
	\property Staff.transposing = 3
	\notes \type Voice=corni < 
		\time 4/4;
		\$corno1
		\$corno2
	>
>

