\header{
filename =	 "violino-1.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.9";

violino1 = \melodic{
	\octave c';	
	c1\ff ~ | c | <as'4-. c'-. f-.> r r2 | r1 |
	c1 ~ | c | <d''4-. d'-. d-.> r r2 | r1 |
	c1 ~ | c | <es''4-. es'-.> r r2 | r1 |
	<g'4-. c'-. es-.> r r2 | <b4-. d-. 'g-.> r r2 |
	[c8-.\p es-.] [es()d] [d-. g-.] g4-"ten." |
	[c8-. es-.] [es()d] [d-. g-.] g4-"ten." |
	[c8-. es-.] [es()d] [d-. as-.] [as()g] |
	[g8-. c'-.] [c'()b] [b-. f'-.] [f'()es'] |
	[es'8-.-"cresc." as'-.] as'4. g'8-. g'4 ~ |
	[g'8 fis'] fis'4 ~ fis' g'-.\f |
}

$violino1_staff = \type Staff = violino1 <
	\property Staff.instrument = "violin"
	\melodic< 
		\global;
		\$violino1
	>
>
