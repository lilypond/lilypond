\header{
filename =	 "violino-2.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.10";

violino2 = \melodic{
	\octave c';	
	c1\ff ~ | c | <as'4-. c'-. f-.> r r2 | r1 |
	c1 ~ | c | <as'4-. b-. d-.> r r2 | r1 |
	c1 ~ | c | <c''4-. es'-. fis-.> r r2 | r1 |
	<c'4-. es-. 'g-.> r r2 | <b4-. d-. 'g-.> r r2 |
	R1 *3 |
	r2 ['b8-.\p f-.] [f()es] |
	[es8-. as-.] as4.-"cresc." g8-. g4 ~ |
	[g8 fis-.] fis4 ~ fis g-.\f |
}

$violino2_staff = \type Staff = violino2 <
	\property Staff.instrument = "violin"
	\melodic< 
		\global;
		\$violino2
	>
>
