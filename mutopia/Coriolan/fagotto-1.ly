\header{
filename =	 "fagotto-1.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.110";

fagottoI = \notes \relative c {
	R1*2 |
	as'4-.\ff r r2 |
	R1 *3 |
	as4-. r r2 |
	R1*3 |
	a4-. r r2 |
	R1 |
	c4-. r r2 |
	b4-. r r2 |
	R1*5 |
	r2 r4 g4-.\f |
	R1*5 |
	\property VoiceCombineVoice.crescendoText = "cresc."
	\property VoiceCombineVoice.crescendoSpanner = "dashed-line"
	ges'2\p\< f2 ~ |
	f4 \!e-.\f r2 |
	R1 |
	f2\p()es |
	\property VoiceCombineVoice.crescendoText = "cresc."
	\property VoiceCombineVoice.crescendoSpanner = "dashed-line"
	d1\< |
	d |
	d2 d |
	d d |
	\!c1\ff ~ |
	c4(bes as)g|
	f4. r8 es4. r8|
	b'4. r8 c4. r8|
	bes4. r8 as4. r8|
	g4. r8 f4. r8|
	fis2\sf r|
	fis\sf r|
	f\sf r|
	f\sf r|
	e1\sf es\sf|
	d2. es4\sf(|
	)d2. es4\sf(|
	)d2. es4\sf(|
	)d2. es4\sf(|
	)d4 r r2|
	R1*9|
	\clef "tenor";
	\property VoiceCombineVoice.crescendoText = "cresc."
	\property VoiceCombineVoice.crescendoSpanner = "dashed-line"
	g'2.(as4 \p\<|
	g f d)bes
	\clef "bass";
	|
	\!bes2\ff as|
	g c~|
	c1\p~|
	c~|
	c~|
	c~|
	\property VoiceCombineVoice.crescendoText = "cresc."
	\property VoiceCombineVoice.crescendoSpanner = "dashed-line"
	c\<~|
	c|
	\!c2\ff bes|
	a d~|
	d1~|
	d~|
	d2.()f4|
	f(es2)d4|
	d(c2)bes4|
	bes(a bes)g|
	fis r8 a\pp a4. a8|
	r4 r8 a a4. a8|
	\property VoiceCombineVoice.crescendoText = "cresc."
	\property VoiceCombineVoice.crescendoSpanner = "dashed-line"
	r4 r8 bes\< bes4. bes8|
	r4 r8 bes bes4. bes8|
	r4 r8 c8 c4. c8|
	r4 r8 d d4. d8|
	\!es2.\ff()d4|
	c(bes c)bes|
	es2.()d4|
	c(bes c)bes|
	d2.()es4|
	d( c d)c|
	d2.()es4|
	d( c d)c|
	r r8 c c4\sf()des|
	r r8 c c4\sf()des|
	r r8 c c4\sf()des|
	r r8 c c4\sf()des|
	es1|
	d2 des|
	c1~|
	c|
	d4 r es r|
	c r d r|
	g,4\p r r2|
	R1*7|

	%% 110|
	% copied from cello, copied from viola|
	[g8 bes d es][f, a cis d] ||
	[es,8:8 g: b: c!:][d,: f: a: bes:] ||
	[c,: es: g: a:][cis,: e: g: bes:] ||
	[d,: g: bes: d:][d,: fis: a: d:] | |
	g,2 ~ g4. g8 ||
	%115|
	g2 ~ g4. g8 ||
	g4. g8 g4 r8 g ||
	g4 r8 g g4 r8 g ||

	g4\p r r2|


	R1*3|
	r4 r8 des' c4 r8 g|
	as4 r r2|
	r4 r8 des c4 r8 g|
	e4 r r2|
	R1*3|
	r4 r8 f' es4 r8 bes|
	\property VoiceCombineVoice.crescendoText = "cresc."
	\property VoiceCombineVoice.crescendoSpanner = "dashed-line"
	c4\< r4 r2|
	R1*2|
	r4 r8 ges' f4 r8 c|
	des4 r bes2|
	c1|

	% 136 a deux
	\!des2\p( c|
	bes as|
	ges f|
	)es as,|

	%% 140 copied from cello (from viola)|
	[des8\f f a bes][c, es g as]||
	[bes,8:8 des: f: ges:][as,: c: e: f:]||
	[g,: bes: c: des:][f,: bes: c: des:]||
	[e,: g: bes: des:][c: e: g: c:]||

	\property VoiceCombineVoice.crescendoText = "cresc."
	\property VoiceCombineVoice.crescendoSpanner = "dashed-line"
	c1\p\< ~|
	c1~|
	c1~|
	c|

	%%148 copied from cello (from viola)|
	[\!f,8:\ff as: c: des:][es,: g: b: c:]||
	[des,: f: a: bes:][c,: es: g: as:]||
	%150|
	% r[des,16 des][e e g g]r8[c,16 c][e e f f]||
	[bes, des e g][as, c e f]||
	% r8[des16 des][f f g g][c, c e e][g g c c]||
	[bes, des f g][c, e g c]||
	f,1\ff~|
	f|
	des'4 r r2|
	R1|
	f,1~|
	f|
	b2\sf r|
	b2\sf r|
	R1*3|
	bes2\ff r|
	bes\sf r|
	R1*2|
	r2 r4 e,4\ff(|
	)f2. e4\sf(|
	)f2. e4\sf(|
	)f2. e4\sf(|
	)f2. fis4\sf(|
	)g2. fis4\sf(|
	)g2. fis4\sf(|
	)g2. fis4\sf(|
	)g2. fis4\sf(|
	)g4 r r2|
	R1|
	g,1\p~|
	g~|
	g~|
	g|
	e''2.(f4|
	e d b)g|
	g'2.(a4|
	g2)f|
	e2.(f4|
	e d b)g|
	g2\ff f|
	e a~|
	a1\p~|
	a~|
	a~|
	a|
	f'2.(g4|
	g e cis)a|
	a2\ff g|
	fis b~|
	b1\p~|
	b~|
	b~|
	b|
	c2.()bes4|
	bes\p(as2)g4|
	g'4(f2)es4|
	es(d es)c|
	b r8 d\p d4. d8|
	r4 r8 d d4. d8|
	\property VoiceCombineVoice.crescendoText = "cresc."
	\property VoiceCombineVoice.crescendoSpanner = "dashed-line"
	r4 r8 es\< es4. es8|
	r4 r8 es es4. es8|
	r4 r8 %
	\clef "tenor"; %
	f f4. f8|
	r4 r8 g g4. g8 %
	\clef "bass"; %
	|
	\!f2.\ff()es4|
	d(c d)c|
	f2.\ff()es4|
	d(c d)c|
	bes2.()as4|
	g(f g)f|
	bes2.()as4|
	g(f g)f|
	r r8 des' des4\sf()es|
	r r8 des des4\sf()es|
	r r8 des des4\sf()es|
	r r8 des des4\sf()es|
	des2 b|
	c a|
	b c|
	d c|
	g4 r as r|
	f r g r|
	c,4 r r2|
	R1*7|

	%%238 copied from cello from viola|
	[c8\f e g as][bes, d fis g]||
	[as, c e f!][g, bes d es]||

	f4 r r2|

	R1*7|
	es'2.\p(f4|
	es d b)g|
	g'2.(as4|
	g2)f|
	\property VoiceCombineVoice.crescendoText = "cresc."
	\property VoiceCombineVoice.crescendoSpanner = "dashed-line"
	es2.\<( f4|
	es d b)g|
	R1|
	\!c1\f|
	des\p|
	d\f|
	es\p|
	e\f|
	\property VoiceCombineVoice.crescendoText = "cresc."
	\property VoiceCombineVoice.crescendoSpanner = "dashed-line"
	as,4.\p\< as8 g4. g8|
	f4. f8 es4. es8|
	as4. as8 g4. g8|
	f4. f8 es4. es8|
	\!f4.\ff as8 as2\sf|
	r4 r8 as as2\sf|
	r4 r8 bes bes2\sf|
	r4 r8 bes bes2\sf|
	r4 r8 c c2\sf|
	r4 r8 d d2\sf|

	%% 270 copied from cello |
	[c,8-"sempre"\ff b c d][es\ff d es c]||
	[b c d c][b c b g]||
	[c8 b c d][es d es c]||
	[b c d c][b c b g]||
	[c b c g][c b c g]||
	%275|
	[c b c g][c b c g]||


	c1~ |
	c|
	f4 r r2|
	R1|
	c1~ |
	c|
	f4 r r2|
	R1|
	c1~ |
	c|
	es'4-. d-. r2|
	R1|
	g,4-. g-. r2|
	as4-. bes-. r2|
	\property VoiceCombineVoice.decrescendoText = "dim."
	\property VoiceCombineVoice.decrescendoSpanner = "dashed-line"
	c4\> r d r|
	R1|
	\!c,4\p r r2|
	R1|
	g4 r r2|
	R1|
	c4\pp r r2|
	\property VoiceCombineVoice.decrescendoText = "sempre pi\\`u piano"
	% urg, "" is assumed to be hairpin...
	%\property VoiceCombineVoice.decrescendoSpanner = ""
	\property VoiceCombineVoice.decrescendoSpanner = "dashed-line"
	r r4 g\p\>~|
	g1~|
	g~|
	g~|
	g~|
	g~|
	g~|
	g~|
	g~|
	g~|
	\!g\>~|
	g~|
	g|
	\!c4\pp r r2|
	R1*4|
}

