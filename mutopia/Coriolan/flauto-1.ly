\header{
filename =	 "flauto-1.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.0.7";

flauto1 = \notes \relative c {
	R1 *2 | c'''4-.\ff r r2 | R1 *3 | d4-. r r2 | R1 *3 |
	es4-. r r2 | r1 | g4-. r r2 | b,4-. r r2 | R1 *4 |
	r2 es\p_"\ \ \ cresc." ( | 
	%20
	)dis2. es4-.\f |
	R1*5 | r2 f\p_"\ \ \ cresc." ~ | 
	f4 e-.\f r2 | R1 | f2()es | 
	%30
	d1-"cresc."  | d1 | 
	d2 d2 | d2 d2 | f1\ff % ~
	%35
}

