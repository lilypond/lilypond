\header{
filename =	 "corno-2.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.0.7";

corno2 = \notes \relative c {
	R1 *2 | d''4-.\ff r r2 | R1 *3 | d4-. r r2 | R1 *3 |
	c4-. r r2 | r1 | e,4-. r r2 | e4-. r r2 | R1 *5 |
	%20
	r2 r4 e4-.\f | R1*6 | r4 g-.\f r2 | R1*2 | 
	% `a 2
	% urg, yes this is a-deux, but lily doesn't know about I. / a2 yet.
%	r8 f'\p_"\ \ \ cresc." f2 e4 | r8 f f2 e4 |
%	f4. e8 f4. e8 | f4. e8 f4. e8 | 
	R1*4 |
	d1\ff % ~ |
	%35
}

