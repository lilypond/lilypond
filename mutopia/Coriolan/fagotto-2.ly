\header{
filename =	 "fagotto-2.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.4";

fagotto2 = \notes \relative c {
	R1*2 |
	f4-.\ff r r2 |
	R1*3 |
	f4-. r r2 |
	R1*3 |
	fis4-. r r2 |
	r1 |
	g4-. r r2 |
	g4-. r r2 |
	R1*5 |
	r2 r4 g,4-. |
	R1*6 |
	r4 g'\f r2 |
	R1*6 |
	as1\ff ~ |
	as4(g f)es|
	d4. r8 c4. r8|
	b4. r8 c4. r8|
	bes4. r8 as4. r8|
	g4. r8 f4. r8|
	fis2\sf r|
	fis\sf r|
	f\sf r|
	f\sf r|
	e1\sf |
	es\sf|
	d2. es4\sf(|
	)d2. es4\sf(|
	)d2. es4\sf(|
	)d2. es4\sf(|
	)d4 r r2|
	R1*9|
	\clef "tenor";
	\property VoiceTwo.crescendoText = "cresc."
	\property VoiceTwo.crescendoSpanner = "dashed-line"
	g''2.(as4 \p\<||
	g f d)bes||
	\clef "bass";
	
	\!g2\ff f|
	e c~|
	c1\p~|
	c~|
	c~|
	c~|
	\property VoiceTwo.crescendoText = "cresc."
	\property VoiceTwo.crescendoSpanner = "dashed-line"
	c\<~|
	c|
	\!c2\ff g'|
	fis d|
	\property VoiceTwo.crescendoText = "cresc."
	\property VoiceTwo.crescendoSpanner = "dashed-line"
	g,4 \p\< r r r8 g(|
	)c4 r r r8 c(|
	\!)b1\sf\>|
	\!c4\p r4 r2|
	R1*2|
	r4 r8 fis\pp fis4. fis8|
	r4 r8 fis fis4. fis8|
	\property VoiceTwo.crescendoText = "cresc."
	\property VoiceTwo.crescendoSpanner = "dashed-line"
	r4 r8 g\< g4. g8|
	r4 r8 g g4. g8|
	r4 r8 a a4. a8|
	r4 r8 bes bes4. bes8|
	\!c2.\ff()bes4|
	a(g a)g|
	c2.()bes4|
	a(g a)g|
	b2.()c4|
	d,(c d)c|
	b'2.()c4|
	d,(c d)c|
	r4 r8 c'8 c4\sf()des|
	r4 r8 c8 c4\sf()des|
	r4 r8 c8 c4\sf()des|
	r4 r8 c8 c4\sf()des|
	c1|
	b2 bes|
	as g|
	fis es|
	d4 r es r |
	c r d r|
	g1\p|
	fis|
	g\fp|
	fis|
	g2\fp(f|
	es d|
	c bes|
	)a d|

	%a deux|
	%% 110|
	% copied from cello, copied from viola||
	[g8 bes d es][f, a cis d] |||
	[es,8:8 g: b: c!:][d,: f: a: bes:] |||
	[c,: es: g: a:][cis,: e: g: bes:] |||
	[d,: g: bes: d:][d,: fis: a: d:] | ||
	g,,2 ~ g4. g8 |||
	%115||
	g2 ~ g4. g8 |||
	g4. g8 g4 r8 g |||
	g4 r8 g g4 r8 g |||

	g'1\p||


	fis|
	g|
	f|
	e|
	f|
	e|
	c|
	f|
	e|
	f|
	g|
	\property VoiceTwo.crescendoText = "cresc."
	\property VoiceTwo.crescendoSpanner = "dashed-line"
	as\<|
	g|
	as|
	a|
	bes|
	c|

	%a deux|
	\!des2\p( c||
	bes as||
	ges f||
	)es as,||

	%% 140 copied from cello (from viola)||
	[des8\f f a bes][c, es g as]|||
	[bes,8:8 des: f: ges:][as,: c: e: f:]|||
	[g,: bes: c: des:][f,: bes: c: des:]|||
	[e,: g: bes: des:][c: e: g: c:]|||

	f,4\p r r2|
	R1*3|

	% a deux|
	%%148 copied from cello (from viola)||
	[f8:\ff as: c: des:][es,: g: b: c:]|||
	[des,: f: a: bes:][c,: es: g: as:]|||
	%150||
	% r[des,16 des][e e g g]r8[c,16 c][e e f f]|||
	[bes, des e g][as, c e f]|||
	% r8[des16 des][f f g g][c, c e e][g g c c]|||
	[bes, des f g][c, e g c]|||
	f,1\ff~|
	f|
	bes,4-. r r2|
	R1|
	f'1\ff~|
	f|
	b,2\sf r|
	b\sf r|
	R1*3|
	bes2 r\ff|
	bes2 r\sf|
	R1*2|
	r2 r4 bes\ff(|
	)as2. bes4\sf(|
	)as2. bes4\sf(|
	)as2. bes4\sf(|
	)as2. c4\sf(|
	)b2. c4\sf(|
	)b2. c4\sf(|
	)b2. c4\sf(|
	)b2. c4\sf(|
	)b4 r r2|
	R1|
	% a deux|
	g1\p~|
	g~|
	g~|
	g~|
	\property VoiceTwo.crescendoText = "cresc."
	\property VoiceTwo.crescendoSpanner = "dashed-line"
	g\<~|
	g~|
	g~|
	g~|
	g~|
	g|
	\!e'2\ff d|
	cis a~|
	a1\p~|
	a~|
	a~|
	a~|
	\property VoiceTwo.crescendoText = "cresc."
	\property VoiceTwo.crescendoSpanner = "dashed-line"
	a\<~|
	a|
	\!f'2\ff e|
	dis b~|
	b1\p|
	a\<|
	g|
	f|
	\!es4\f r r2|
	R1*3|
	r4 r8 b'' b4. b8|
	r4 r8 b b4. b8|
	\property VoiceTwo.crescendoText = "cresc."
	\property VoiceTwo.crescendoSpanner = "dashed-line"
	r4 r8 c\< c4. c8|
	r4 r8 c c4. c8|
	\clef "tenor";
	r4 r8 d d4. d8|
	r4 r8 es es4. es8|
	\clef "bass"; 
	\!f,2.\ff()es4|
	d(c d)c|
	f2.\ff()es4|
	d(c d)c|
	bes2.()as4|
	g(f g)f|
	bes2.()as4|
	g(f g)f|
	f4 r r2|
	f4 r r2|
	f4 r r2|
	f4 r r2|
	f'2 f|
	e es|
	d c|
	b' as|
	% a deux|
	g4 r as r|
	f r g r|
	c,1\p|
	b|
	c|
	b|
	c2\fp( bes|
	as g|
	f es'|
	)d g,|

	%a deux|
	%%238 copied from cello from viola||
	[c8\f e g as][bes, d fis g]|||
	[as, c e f!][g, bes d es]|||

	f4 r r2||

	R1*14|
	ges\f|
	f\p|
	as\f|
	g\p|
	bes\f|
	%a deux|
	\property VoiceTwo.crescendoText = "cresc."
	\property VoiceTwo.crescendoSpanner = "dashed-line"
	as4.\p\< as8 g4. g8|
	f4. f8 es4. es8|
	as4. as8 g4. g8|
	f4. f8 es4. es8|
	\!f4.\sf f8 f2\sf|
	r4 r8 f f2\sf|
	r4 r8 g g2\sf|
	r4 r8 g g2\sf|
	r4 r8 as as2\sf|
	r4 r8 b b2\sf|

	% a deux|
	%% 270 copied from cello ||
	[c,8-"sempre"\ff b c d][es\ff d es c]|||
	[b c d c][b c b g]|||
	[c8 b c d][es d es c]|||
	[b c d c][b c b g]|||
	[c b c g][c b c g]|||
	%275||
	[c b c g][c b c g]|||

	% a deux|
	c1~|
	c|
	f,4-. r r2|
	R1|
	c'1~|
	c|
	f,4-. r r2|
	R1|
	c'1~|
	c|
	c'4-. b-. r2|
	R1|
	es,4-. e-. r2|
	f4-. g-. r2|
	\property VoiceTwo.decrescendoText = "dim."
	\property VoiceTwo.decrescendoSpanner = "dashed-line"
	as4\> r b r|
	R1|
	%a deux|
	\!c,4\p r r2|
	R1|
	g4 r r2|
	R1|
	c4\pp r r2|
	R1*18|
}

