\header{
filename =	 "clarinetti.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "1.0.7";

\include "clarinetto-1.ly"
\include "clarinetto-2.ly"

$clarinetti_staff = \type Staff = clarinetti <
	\property Staff.midi_instrument = "clarinet"
	\property Staff.instrument = "2 Clarinetti (B\\textflat)"
	\property Staff.instr = "Cl. (B\\textflat)"
	% urg: can't; only My_midi_lexer:<non-static> () parses pitch?
	%\property Staff.transposing = "bes"
	\property Staff.transposing = -3
	\notes \type Voice=one < 
		\time 4/4;
		\key F;
		\$clarinetto1
		\$clarinetto2
	>
>

