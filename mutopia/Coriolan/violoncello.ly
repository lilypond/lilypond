\header{
filename =	 "violoncello.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.10";

violoncello = \melodic{
	\octave c;
	c1\ff ~ | c | 'f4-. r r2 | r1 |
	c1\ff ~ | c | 'f4-. r r2 | r1 |
	c1\ff ~ | c | 'fis4-. r r2 | r1 |
	'g4-. r r2 | 'g4-. r r2 |
	c4\p r4 r2 | c4 r4 r2 | c4 r4 r2 | r1 |
	c'4.-"cresc." c'8 bes4. bes8 | as4. as8 g4\f r4 |
}

$violoncello_staff = \type Staff = violoncello <
	\property Staff.instrument = "cello"
	\clef "bass";
	\melodic< 
		\global;
		\$violoncello
	>
>
