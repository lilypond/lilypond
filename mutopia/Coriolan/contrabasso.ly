\header{
filename =	 "contrabasso.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.59";

contrabasso = \notes \relative c {
	% copied 21 from cello
	c1\ff ~ |
	c |
	f,4-. r r2 |
	r1 |
	c'1\ff ~ |
	c |
	f,4-. r r2 |
	r1 |
	c'1\ff ~ |
	c |
	fis,4-. r r2 |
	r1 |
	g4-. r r2 |
	g4-. r r2 |
	c4\p r4 r2 |
	c4 r4 r2 |
	c4 r4 r2 |
	r1 |
	\property StaffCombineVoice.crescendoText = "cresc."
	\property StaffCombineVoice.crescendoSpanner = "dashed-line"
	c'4.\< c8 bes4. bes8 |

	%20
	as4. as8 \!g4\f r4 |
	R1|

	bes,4\p r4 r2 |
	bes4   r4 r2 |
	bes4   r4 r2 |
	%25
	R1 |

	% copied 14 from cello

	\property StaffCombineVoice.crescendoText = "cresc."
	\property StaffCombineVoice.crescendoSpanner = "dashed-line"
	bes'4.\< bes8 as4. as8 |
	g4 \!g4-.\f r2 |
	R1
	as4.\p as8 g4. g8 |
	f2.-"cresc" g4 |
	f2. g4 |
	f4. g8 f4. g8 |
	f4. g8 f4. g8 |
	as1\ff ~ |
	%35
	as4(g f)es|
	d4. r8 c4. r8 |
	b4. r8 c4. r8 |
	bes4. r8 as4. r8 |
	g4. r8 f4. r8 |
	%40
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
	\property StaffCombineVoice.crescendoText = "cresc."
	\property StaffCombineVoice.crescendoSpanner = "dashed-line"
	)es4\< r r r8 es(|
	)as,4 r r r8 as(|
	)g4 r r r8 g(|
	)d'4 r r r8 d(|
	%60
	)es4 r r r8 es(|
	)as,4 r r r8 as(|
	\!)g2\ff f |
	e c' |
	f,4\p r r r8 f(|
	%65
	)bes4 r r r8 bes(|
	)as4 r r r8 as(|
	)e4 r r r8 e(|
	\property StaffCombineVoice.crescendoText = "cresc."
	\property StaffCombineVoice.crescendoSpanner = "dashed-line"
	)f4\< r r r8 f(|
	)bes4 r r r8 bes(|
	%70
	\!)as2\ff g |
	fis d' |
	\property StaffCombineVoice.crescendoText = "cresc."
	\property StaffCombineVoice.crescendoSpanner = "dashed-line"
	g,4\p\< r r r8 g(|
	)c4 r r r8 c(|
	% )<b1\sf\> { s2 s4 \!s8}> |
	\!)b1\sf |
	%75
	% copied 3 from cello
	c4\p r d r |
	es r d r |
	c r cis r |
	d4\pp r d r |
	d r d r |
	%80
	\property StaffCombineVoice.crescendoText = "cresc."
	\property StaffCombineVoice.crescendoSpanner = "dashed-line"
	d r d\< r |
	d r d r |
	d r d r |
	d r d r |
	% copied 8 from cello
	\!c'2.\ff()bes4 |
	%85
	a(g a)g|
	c2.()bes4 |
	a(g a)g|
	f!2.()es4 |
	d(c d)c|
	%90
	f2.()es4 |
	d(c d)c|
	c4 r r2 |
	c4 r r2 |
	c4 r r2 |
	%95
	c4 r r2 |
	% copied 6 from cello |
	c4. c'8 c,4. c'8 |
	b,4. b'8 bes,4. bes'8 |
	as,4. as'8 g,4. g'8 |
	fis,4. fis'8 es4. es8 |
	%100
	d4 r8 d es4 r8 es |
	c4 r8 c d4 r8 d |
	g,4\p r r2 |
	r2 r4 r8 fis\f |
	%105
	g4 r r2 |
	r2 r4 r8 fis\f |
	g4 r r2 |
	R1*3 |
	% copied 8 from cello
	%110
	[g'8:8 bes d es][f, a cis d] |
	[es,8: g: b: c!:][d,: f: a: bes:] |
	[c,: es: g: a:][cis,: e: g: bes:] |
	[d,: g: bes: d:][d,: fis: a: d:] |
	g,,2 ~ g4. g8 |
	%115
	g2 ~ g4. g8 |
	g4. g8 g4 r8 g |
	g4. g8 g4 r8 g |
	g4\p r r2 |
	% urg, this is *rest*, not skip: 
	% need own staff for rests, mustn't collapse to one cello staff!
	%120 - 1
	R1*21 |
	% copied 4 frorm cello
	[des'8\f f a bes][c, es g as]|
	[bes,8:8 des: f: ges:][as,: c: e: f:]|
	[g,: bes: c: des:][f,: bes: c: des:]|
	[e,: g: bes: des:][c: e: g: c:]|
	f,4\p r r2 |
	R1*3 |
	% copied 10 from cello|
	[f8:\ff as: c: des:][es,: g: b: c:]|
	[des,: f: a: bes:][c,: es: g: as:]|
	%150
	% r[des,16 des][e e g g]r8[c,16 c][e e f f]|
	[bes, des e g][as, c e f]|
	% r8[des16 des][f f g g][c, c e e][g g c c]|
	[bes, des f g][c, e g c]|
	f,1\ff ~ |
	f |
	bes,4-. r r2
	%155
	R1 |
	f'1\ff ~ |
	f |
	b,2\sf r |
	b2\sf r |
	%160
	R1*3 |
	bes2\ff r |
	bes2\sf r |
	R1*2 |
	r2 r4 bes\ff( |
	)as2. bes4\sf(|
	)as2. bes4\sf(|
	)as2. bes4\sf(|
	)as2. c4\sf(|
	)b2. c4\sf(|
	)b2. c4\sf(|
	)b2. c4\sf(|
	)b2. c4\sf(|
	)b4 r r2 |
	R1 |
	c4\p r r r8 c(|
	)f,4 r r r8 f(|
	%180
	)e4 r r r8 e(|
	)b'4 r r r8 b(|
	\property StaffCombineVoice.crescendoText = "cresc."
	\property StaffCombineVoice.crescendoSpanner = "dashed-line"
	)c4\< r r r8 c(|
	)f,4 r r r8 f(|
	)e4 r r r8 e'(|
	%185
	)b4 r r r8 b(|
	)c4 r r r8 c(|
	)f,4 r r r8 f(|
	\!)e2\ff d' |
	cis a |
	%190
	d4\p r r r8 d(|
	)g,4 r r r8 g(|
	)f4 r r r8 f(|
	)cis'4 r r r8 cis(|
	\property StaffCombineVoice.crescendoText = "cresc."
	\property StaffCombineVoice.crescendoSpanner = "dashed-line"
	)d4\< r r r8 d(|

	%195
	)g,4 r r r8 g( |
	\!)d2\ff e' |
	dis b |
	es4\p r r r8 es(|
	)a,4 r r r8 a(|
	)g1 |

	%200
	f |
	% copied 18 from cello
	es'!4\fp r e r |
	f r g r |
	as r g r |
	f r fis r |

	%205
	g r g r |
	g r g r |
	\property StaffCombineVoice.crescendoText = "cresc."
	\property StaffCombineVoice.crescendoSpanner = "dashed-line"
	g r g\< r |
	g r g r |
	
	%210
	g r g r |
	g r g r |
	\!f2.\ff()es4 |
	d( c d )c |
	f2.\ff()es4 |

	%215
	d( c d )c |
	bes2.\ff()as4 |
	g( f g )f |
	bes2.\ff()as4 |
	g( f g )f |

	%220
	f4 r r2 |
	f'4 r r2 |
	f4 r r2 |
	f4 r r2 |
	% copied 6 from cello |
	f4. f'8 f,4. f'8 |
	e,4. e'8 es,4. es'8 |
	d,4. d'8 c,4. c'8 |
	b,4. b'8 as,4. as'8 |
	g,4 r8 g as4 r8 as |

	%230
	f4 r8 f g4 r8 g |
	c4\p r r2 |
	r2 r4 r8 b\f |
	c4 r r2 |
	r2 r4 r8 b\f |
	%235
	c4 r r2 |
	R1*3 |

	%copied 59 from cello
	[c8\f e g as][bes, d fis g]|

	%240
	[as, c e f!][g, bes d es]|
	f4 r r2 |
	R1*3 |
	c4\p r r r8 c(|

	%245
	)f4 r r r8 f(|
	)e4 r r r8 e(|
	)b4 r r r8 b(|
	)c4 r r r8 c(|
	)f4 r r r8 f(|

	%250
	)es4 r r r8 es(|
	)b4 r r r8 b(|
	\property StaffCombineVoice.crescendoText = "cresc."
	\property StaffCombineVoice.crescendoSpanner = "dashed-line"
	)c4\< r r r8 c(|
	)f4 r r r8 f,(|
	)es1 |
	
	%255
	\!ges\f |
	f\p |
	as\f |
	g!\p |
	bes\f |

	%260
	\property StaffCombineVoice.crescendoText = "cresc."
	\property StaffCombineVoice.crescendoSpanner = "dashed-line"
	as4.\p\< as'8 g4. g8 |
	f4. f8 es4. es8 |
	as4. as8 g4. g8 |
	f4. f8 es4. es8 |
	\!f,4.\ff f'8 f2\sf |
	
	%265
	f,4. f'8 f2\sf |
	g,4. g'8 g2\sf |
	g,4. g'8 g2\sf |
	as,4. as'8 as2\sf |
	b,4. b'8 b2\sf |
	
	%270
	[c,8-"sempre"\ff b c d][es\ff d es c]|
	[b c d c][b c b g]|
	[c8 b c d][es d es c]|
	[b c d c][b c b g]|
	[c b c g][c b c g]|

	%275
	[c b c g][c b c g]|
	c1 ~ |
	c |
	f4-. r r2 |
	R1 |

	%280
	c1 ~ |
	c |
	f4-. r r2 |
	R1 |
	c1 ~ |

	%285
	c |
	fis4-. f-. r2 |
	R1|
	es4-. e-. r2 |
	f,4-. g-. r2 |

	%290
	\property StaffCombineVoice.decrescendoText = "dim."
	\property StaffCombineVoice.decrescendoSpanner = "dashed-line"
	as4\> r b r |
	R1 |
	\!c4\p r r2|
	R1 |
	g4-"pizz." r r2 |
	
	%295
	R1 |
	c4 r r2 |
	r2 r4 f, |
	c' r r2 |
	r2 r4 f, |

	%300
	c' r r2 |
	R1 |
	f,4 r r2 |
	R1 |
	c'4 r r2 |

	%305
	R1 |
	f,4 r r2 |
	R1*3 |

	%310
	c'4\pp r r2 |
	R1 |
	c4 r r2 |
	c4 r r2 |
	c4 r r2 |
}
