\header{
filename =	 "contrabasso.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.0.7";

contrabasso = \notes \relative c {
%	\translator Staff=violoncello
	\skip 1*21;
%	\translator Staff=contrabasso
	bes4\p r4 r2 |
	bes4   r4 r2 |
	bes4   r4 r2 |
	%25
	R1 |
	\skip 1*14;
	fis2\sf r |
	fis2\sf r |
	f!2\sf r |
	f2\sf r |
	e2\sf r |
	%45
	es!2\sf r |
	des2. es4\sf(|
	)d2. es4\sf(|
	)d2. es4\sf(|
	)d2. es4\sf(|
	% 50
	)d4 r r2 |
	R1
	es'4\p r r r8 es(|
	)as,4 r r r8 as(|
	)g4 r r r8 g(|
	)d'4 r r r8 d(|
	)es4-"cresc." r r r8 es(|
	)as,4 r r r8 as(|
	)g4 r r r8 g(|
	)d'4 r r r8 d(|
	%60
	)es4 r r r8 es(|
	)as,4 r r r8 as(|
	)g2\ff f | e c' |
	f,4\p r r r8 f(|
	%65
	)bes4 r r r8 bes(|
	)as4 r r r8 as(|
	)e4 r r r8 e(|
	)f4-"cresc." r r r8 f(|
	)bes4 r r r8 bes(|
	%70
	)as2\ff g | fis d' |
	g,4\p-"cresc." r r r8 g(|
	)c4 r r r8 c(|
	% )<b1\sf\> { s2 s4 \!s8}> |
	)b1 |
	%75
	\skip 1*3; |
	d4\pp r d r | d r d r | d r d-"cresc." r | 
	d r d r | d r d r | d r d r | 
	\skip 1*8; |
	c4 r r2 | c4 r r2 | c4 r r2 | c4 r r2 |
	\skip 1*6; |
	g4\p r r2 | r2 r4 r8 fis\f |
	g4 r r2 | r2 r4 r8 fis\f |
	g4 r r2 | 
	R1*3 | 
	\skip 1*8; | g4\p r r2 |
	% urg, this is *rest*, not skip: 
	% need own staff for rests, mustn't collapse to one cello staff!
	%120 - 1
	R1*21 |
	% play with cello
	\skip 1*4; |
	f'4\p r r2 |
	R1*3 |
	\skip 1*10; |
	b,2\sf r | b2\sf r | R1*3 |
	bes2\ff r | bes2\sf r | R1*2 | 
	r2 r4 bes\ff( |
	)as2. bes4\sf(|
	)as2. bes4\sf(|
	)as2. bes4\sf(|
	)as2. c4\sf(|
	)b2. c4\sf(|
	)b2. c4\sf(|
	)b2. c4\sf(|
	)b2. c4\sf(|
	)b4 r r2 | R1 |
	c4\p r r r8 c(|
	)f,4 r r r8 f(|
	%180
	)e4 r r r8 e(|
	)b'4 r r r8 b(|
	)c4-"cresc." r r r8 c(|
	)f,4 r r r8 f(|
	)e4 r r r8 e'(|
	%185
	)b4 r r r8 b(|
	)c4 r r r8 c(|
	)f,4 r r r8 f(|
	)e2\ff d' | cis a | 
	%190
	d4\p r r r8 d(|
	)g,4 r r r8 g(|
	)f4 r r r8 f(|
	)cis'4 r r r8 cis(|
	)d4-"cresc." r r r8 d(|
	%195
	)g,4 r r r8 g( |
	)d2\ff e' | dis b | 
	e4\p r r r8 e(|
	% )a4\< r r r8 a(|
	%200
	% g1 | <f {s2 s4 \!s8}> | 
	)a4 r r r8 a(|
	%200
	)g1 | f | 
	\skip 1*18; |
	%220
	f4 r r2 | f'4 r r2 | f4 r r2 | f4 r r2 | 
	\skip 1*6; |
	c,4\p r r2 | r2 r4 r8 b\f |
	c4 r r2 | r2 r4 r8 b\f |
	c4 r r2 | 
	R1*3 |
	\skip 1*59; |
	r2 r4 f, | c' r r2 |
	r2 r4 f, | c' r r2 | R1 | 
	f,4 r r2 | R1 |
	c'4 r r2 | R1 |
	f,4 r r2 | R1*3 | c'4\pp r r2 | R1 |
	c4 r r2 |
	c4 r r2 |
	c4 r r2 |
}

$contrabasso_staff = \type Staff = contrabasso <
	\property Staff.midi_instrument = "contrabass"
	\property Staff.instrument = "Contrabasso"
	\property Staff.instr = "Cb."
	\clef "bass";
	\notes \type Voice=one< 
		\global
		\$contrabasso
	>
>

