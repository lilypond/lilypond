\header{
filename =	 "contrabasso.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.14";

contrabasso = \melodic{
	\octave c;
	c1\ff ~ | c | f,4-. r r2 | r1 |
	c1\ff ~ | c | f,4-. r r2 | r1 |
	c1\ff ~ | c | fis,4-. r r2 | r1 |
	g,4-. r r2 | g,4-. r r2 |
	c4\p r4 r2 | c4 r4 r2 | c4 r4 r2 | r1 |
	c'4.-"cresc." c'8 bes4. bes8 | as4. as8 g4\f r4 |
}

$contrabasso_staff = \type Staff = contrabasso <
	\property Staff.instrument = "contrabass"
	\clef "bass";
	\melodic< 
		\global;
		\$contrabasso
	>
>
