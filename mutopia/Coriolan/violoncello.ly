\header{
filename =	 "violoncello.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "1.0.7";

violoncello = \notes \relative c {
	c1\ff ~ | c | f,4-. r r2 | r1 | c'1\ff ~ | c | f,4-. r r2 |
	\property Staff.instr = "Vc. \& Cb."
	r1 |
	c'1\ff ~ | c | fis,4-. r r2 | r1 |
	g4-. r r2 | g4-. r r2 |
	c4\p r4 r2 | c4 r4 r2 | c4 r4 r2 | r1 |
	c'4.-"cresc." c8 bes4. bes8 | 
	%20
	as4. as8 g4\f r4 |
	R1 |
	% 4 bars same as violino-1...
	[bes,!8\p-. des-.][des()c][c-. f-.]f4-"ten." |
	[bes,!8\p-. des-.][des()c][c-. f-.]f4-"ten." |
	[bes,!8\p-. des-.][des()c][c-. ges'-.][ges()f] |
	%25
	[f-. bes-.][bes()a][a-. es'-.][es()des]|
	bes4.-"cresc." bes8 as4. as8 | g4 g4-.\f r2 | R1
	as4.\p as8 g4. g8 | f2.-"cresc" g4 | f2. g4 | 
	f4. g8 f4. g8 | 
	f4. g8 f4. g8 | 
	as1\ff ~ |
	%35
	as4(g f)es|
	d4. r8 c4. r8 | b4. r8 c4. r8 | bes4. r8 as4. r8 | g4. r8 f4. r8 |
	%40
	fis2\sf [c'8-. es-.][es()d]
	fis,2\sf [c'8-. es-.][es()d]
	f,!2\sf [b8-. d-.][d()c]
	f,2\sf [b8-. d-.][d()c]
	e,2\sf [bes'!8-. des-.][des()c]
	%45
	es,!2\sf [a!8-. c-.][c()bes] |
	d,4 [a'!8-. c-.][c()bes] es,4\sf(|
	)d4 [a'!8-. c-.][c()bes] es,4\sf(|
	)d4 [a'!8-. c-.][c()bes] es,4\sf(|
	)d4 [a'!8-. c-.][c()bes] es,4\sf(|
	%50
	)d4 r r2 |
	R1 |
	[bes'8\p(g'es g][bes g es)bes~] |
	[bes(f'd f][bes f d)bes~] |
	[bes(g'es g][bes g es)bes~] |
	%55
	[bes(as'f as][bes as f)bes,~] |
	[bes-"cresc."(g'es g][bes g es)bes~] |
	[bes(f'd f][bes f d)bes~] |
	[bes(g'es g][bes g es)bes~] |
	[bes(as'f as][bes as f)bes,~] |
	%60
	[bes(g'es g][bes g es)bes~] |
	[bes(f'd f][bes f d)as] |
	g2\ff f | e c |
	[f8\p(as'f as][c as f)c~] |
	[c(g'e g][c g e)c~] |
	[c(as'f as][c as f)c~] |
	[c(bes'g bes][c bes g)c,~] |
	[c(as'f as][c as f)c~] |
	[c(g'e g][c g e)bes] |
	%70
	as2\ff g | fis d |
	[g8\p-"cresc."(bes' g bes][d bes g)d~] |
	[d8(a' fis a][d a fis)d] |
	[f!8\sf\>(as f as][f g d)\!g] |
	%75
	c,4\p r d r | es r d r | c r cis r | 
	d\pp r8 d d4. d8 |
	r4 r8 d d4. d8 |
	%80
	r4 r8 d d4.-"cresc." d8 |
	r4 r8 d d4. d8 |
	r4 r8 d d4. d8 |
	r4 r8 d d4. d8 |
	c'2.\ff()bes4 | a(g a)g|
	c2.()bes4 | a(g a)g|
	f!2.()es4 | d(c d)c|
	%90
	f2.()es4 | d(c d)c|
	c r8 c' c4\sf()des|
	r r8 c c4\sf()des|
	r r8 c c4\sf()des|
	%95
	r r8 c c4\sf()des|
	c,4. c'8 c,4. c'8 |
	b,4. b'8 bes,4. bes'8 |
	as,4. as'8 g,4. g'8 |
	fis,4. fis'8 es4. es8 |
	%100
	d4 r8 d es4 r8 es |
	c4 r8 c d4 r8 d |
	% copied \transpose c, viola-1(102, 130)
	% except for some abbrevs below...
	[g8\p bes cis d][es d bes g]|
	[fis a cis d][es d a fis\f] |
	[g bes cis\p d][es d bes g]
	%105
	[fis a cis d][es d a fis\f] |
	[g bes cis\p d][f, a cis d] |
	[es, g b c][d, g a bes] |
	% [c, es g a]r[d, fis g] |
	[c, es g a][bes, d fis g] |
	% r[c, es g][d fis a d] |
	[a, c es g][d fis a d] |
	%110
	% cello has eighth notes here...
	% [g,16 g bes bes][d d es es][f, f a a][cis cis d d] |
	% see if this:8 neat trick works...
	[g,:8 bes d es][f, a cis d] |
	[es,8: g: b: c!:][d,: f: a: bes:] |
	[c,: es: g: a:][cis,: e: g: bes:] |
	[d,: g: bes: d:][d,: fis: a: d:] | 
	g,,2 ~ g4. g8 |
	%115
	g2 ~ g4. g8 |
	g4. g8 g4 r8 g |
	g4. g8 g4 r8 g |
	% urg, 2 copied from 102
	[g'8\p bes cis d][es d bes g]|
	[fis a cis d][es d a fis] |
	%120
	[g bes c des][c des bes g]|[f as c des][c des as f]|
	[e g b c][b c g e]|[f as b c][b c as f]|
	[e g b c][b c g e]|
	%125
	[c e fis g][fis g e c]|[f as b c][des c as f]|
	[e g b c][des c g e]|[f as c des][c des as f]|
	[g bes d es][d es bes g]|[as-"cresc." c d es][f es c as]|
	%131
	[g bes d es][f es bes g]|
	% shared with viola until here
	as1 ( | a | bes | )c | des2\p( c | bes as | ges f | )es [as,8 c es as] |
	%140
	% copied from viola-1 (140,152)
	% except cello has eighth notes
	% [des,16\f des f f][a a bes bes][c, c es es][g g as as]|
	[des,8\f f a bes][c, es g as]|
	[bes,8:8 des: f: ges:][as,: c: e: f:]|
	[g,: bes: c: des:][f,: bes: c: des:]|
	[e,: g: bes: des:][c: e: g: c:]|
	%shared with cello from here on
	[f,8\p-"cresc." as c des][es, g b c]|[des, f a bes][c, es g as]|
	% uhuh, still 'same as cello';
	% but now rests for notes that viola cannot reach...
	% r[des, e g]r[c, e f]|
	[bes, des e g][as, c e f]|
	% hmm, don't want to miss more than one note,
	% take bes octave higher
	% r[bes des, f][c e g c]|
	[g, bes des f][c e g c]|
	% cello has eight notes;
	% viola has rest where can't reach	
	[f,:\ff as: c: des:][es,: g: b: c:]|
	[des,: f: a: bes:][c,: es: g: as:]|
	%150
	% r[des,16 des][e e g g]r8[c,16 c][e e f f]|
	[bes, des e g][as, c e f]|
	% r8[des16 des][f f g g][c, c e e][g g c c]|
	[bes, des f g][c, e g c]|
	f,1\ff ~ | f | bes,4-. r r2
	%155
	R1 |
	f'1\ff ~ | f |
	b,2\sf [f'8-. as-.][as()g] |
	b,2\sf [f'8-. as-.][as()g] |
	%160
	R1*3 |
	bes,!2\ff [e8-. g-.][g()f] |
	bes,2\sf [e8-. g-.][g()f] |
	%165
	R1*2 |
	r2 r4 bes,\ff(|
	)as4 [e'8-. g-.][g()f] bes,4\sf(|
	)as4 [e'8-. g-.][g()f] bes,4\sf(|
	%170
	)as4 [e'8-. g-.][g()f] bes,4\sf(|
	)as4 [e'8-. g-.][g()f][fis-. fis-.] |
	g4 [fis8-. as-.][as()g] c,4\sf(|
	)b4 [fis'8-. as-.][as()g] c,4\sf(|
	)b4 [fis'8-. as-.][as()g] c,4\sf(|
	%175
	)b4 [fis'8-. as-.][as()g] c,4\sf(|
	)b4 r r2 | R1 |
	[g8\p(e' c d][g e c )g~] |
	[g(d' b d][g d b )g~] |
	%180
	[g(e' c d][g e c )g~] |
	[g(f' d f][g f d )g,~] |
	[g-"cresc."(e' c d][g e c )g~] |
	[g(d' b d][g d b )g~] |
	[g(e' c d][g e c )g~] |
	%185
	[g(f' d f][g f d )g,~] |
	[g(e' c d][g e c )g~] |
	[g(d' b d][g d b )g~] |
	e2\ff d' | cis a ~ |
	%190
	[a8( f' d f][a f d )a!~] |
	[a( e' cis e][a e c )a!~] |
	[a8( f' d f][a f d )a!~] |
	[a( g' e g][a g e )a,!~] |
	[a8-"cresc."( f' d f][a f d )a!~] |
	%195
	[a( e' cis e][a e c )g] |
	f2\ff e | dis b'~ | 
	[b8\p( g' e g][b g e )b!~] |
	[b8\<( fis' dis fis ][b fis dis )b!~] |
	%200
	[b8\p( g' e g][e g e )g] |
	[f(g f g][f g f)\!g] |
	es!4\fp r e r | f r g r | as r g r | f r fis r | 
	g r g r | g r g r | g r g-"cresc." r | 
	g r g r | g r g r | g r g r | 
	f2.\ff()es4 | d( c d )c |
	f2.\ff()es4 | d( c d )c |
	bes2.\ff()as4 | g( f g )f |
	bes2.\ff()as4 | g( f g )f |
	%220
	r4 r8 f' f4\sf()ges |
	r4 r8 f f4\sf()ges |
	r4 r8 f f4\sf()ges |
	r4 r8 f f4\sf()ges |
	f4. f'8 f,4. f'8 |
	%225
	e,4. e'8 es,4. es'8 |
	d,4. d'8 c,4. c'8 |
	b,4. b'8 as,4. as'8 |
	g,4 r8 g as4 r8 as |
	f4 r8 f g4 r8 g |
	%230
	[c8\p es fis g][as g es c]|
	[b d fis g][as g d b\f]|
	[c es fis\p g][as g es c]|
	[b d fis g][as g d b\f]|
	[c es g\p as][bes, d fis g]|
	%235
	[as, c e f!][g, bes d es]|
	[f, as c d][es g b c]|
	[d, f as c][g, b d g]|
	[c,\f e g as][bes, d fis g]|
	[as, c e f!][g, bes d es]|
	%240
	f4 r r2 | R1*3 | 
	c4\p r r r8 c(|
	)f4 r r r8 f(|
	)e4 r r r8 e(|
	)b4 r r r8 b(|
	)c4 r r r8 c(|
	%250
	)f4 r r r8 f(|
	)es4 r r r8 es(|
	)b4 r r r8 b(|
	)c4-"cresc." r r r8 c(|
	)f4 r r r8 f,(|
	)es1 | ges\f | f\p | as\f | g!\p | 
	%260
	bes\f |
	as4.\p-"cresc." as'8 g4. g8 | f4. f8 es4. es8 |
	as4. as8 g4. g8 | f4. f8 es4. es8 |
	%265
	f,4.\ff f'8 f2\sf | 
	f,4. f'8 f2\sf | 
	g,4. g'8 g2\sf | 
	g,4. g'8 g2\sf | 
	as,4. as'8 as2\sf | 
	%270
	a,4. a'8 a2\sf | 
	[c,8-"sempre" b c d][es\ff d es c]|
	[b c d c][b c b g]|
	[c8 b c d][es d es c]|
	[b c d c][b c b g]|
	[c b c g][c b c g]|
	%275
	[c b c g][c b c g]|
	c1 ~ | c | f4-. r r2 | R1 |
	c1 ~ | c | f4-. r r2 | R1 |
	c1 ~ | c | fis4-. f-. r2 | R1
	es4-. e-. r2 | f,4-. g-. r2 | as4-"dim." r b r | R1 | c4 r r2 R1 |
	g4-"pizz." r r2 | R1 | c4 r r2 | 
	[c8-.-"arco" es-.][es()d][d-. as'-.][as()g] | R1
	\[/3 c,4-"sempre pi\\`u piano" ( es c ~ \] \[/3 c as' )g \] |
	r2 r4 c,( | es2. ) d4 | r2 r4 d( | as'2. )g4 ~ | g1 ( |
	%305
	es | )d( | )as'\> ~ | as ~ | as2.()\!g4 | c,\pp r r2 | R1 |
	c4-"pizz." r r2 |
	c4 r r2 |
	c4 r r2 |
}

$violoncello_staff = \type Staff = violoncello <
	\property Staff.midi_instrument = "cello"
	\property Staff.instrument = "Violoncello"
	\property Staff.instr = "Vc."
	\clef "bass";
	\notes \type Voice=one< 
		\global
		\$violoncello
	>
>

