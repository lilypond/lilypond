\header{
filename =	 "viola.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.10";

viola = \melodic{
	\octave c;
	< { c'1\ff ~ | c' } { c ~ | c } > | f4-. r r2 | r1 |
	% copied 3 bars from three back...
	< { c'1 ~ | c' } { c ~ | c } > | f4-. r r2 | r1 |
	< { c'1 ~ | c' } { c ~ | c } > | fis4-. r r2 | r1 |
	< g4-. g'-. > r r2 | < g4-. g'-. > r r2 |
	% copied from violino-1...
	[c8-.\p es-.] [es()d] [d-. g-.] g4-"ten." |
	[c8-. es-.] [es()d] [d-. g-.] g4-"ten." |
	[c8-. es-.] [es()d] [d-. as-.] [as()g] |
	[g8-. c'-.] [c'()b] [b-. f'-.] [f'()es'] |
	es'4.-"cresc." es'8 d'4. d'8 | c'4. c'8 b4\f r4 |
}

$viola_staff = \type Staff = viola <
	\property Staff.instrument = "viola"
	\clef "alto";
	\melodic< 
		\global;
		\$viola
	>
>
