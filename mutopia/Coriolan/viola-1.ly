\header{
filename =	 "viola-1.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.0.7";

viola1 = \notes \relative c {
	\type Voice=one
	c'1\ff ~ | c | f,4-. r r2 | r1 |
	% copied 3 bars from three back...
	%5
	c'1 ~ | c | f,4-. r r2 | r1 |
	c'1 ~ | c | fis,4-. r r2 | r1 |
	g'4-. r r2 | g4-. r r2 |
	% copied from violino-1...
	%15
	[c,,8-.\p es-.] [es()d] [d-. g-.] g4-"ten." |
	[c,8-. es-.] [es()d] [d-. g-.] g4-"ten." |
	[c,8-. es-.] [es()d] [d-. as'-.] [as()g] |
	[g8-. c-.] [c()b] [b-. f'-.] [f()es] |
	es4.-"cresc." es8 d4. d8 | 
	%20
	c4. c8 b4\f r4 |
	R1 |
	r4 [des,8\p()c][c-. f-.]f4-"ten." |
	r4 [des8\p()c][c-. f-.]f4-"ten." |
	r4 [des8\p()c][c-. ges'-.][ges()f] |
	%25
	[f-. bes-.][bes()a][a-. es'-.][es()des] | des4.-"cresc." des8 c4. c8 |
	bes4 bes4-.\f r2 |
	R1
	% same rhythm as violins...
	r8 c-.\p c4. c8-. c4~ | [c8-"cresc." c-.]c2.~ |
	%30
	[c8 c-.]c2.~ | [c8 c-.] c4. c8-. c4~ | [c8 c-.] c4. c8-. c4 |
	[c16\ff c c c][c c c c]c2:16 |
      	%35
	[c16 c c c][bes bes bes bes][as as as as][g g g g] |
	g2:16 g: | g: g: | c: c: | e: f: |
	%40
	c2\sf [c,8-. es-.][es()d] |
	c'2\sf [c,8-. es-.][es()d] |
	b'2\f r8 [d,-. d()c] |
	b'2\f r8 [d,-. d()c] |
	b'2\f r8 [des,-. des()c] |
	%45
	a'2\sf [a8-. c-.][c()bes] |
	r4 [a8-. c-.][c()bes] r4 |
	r4 [a8-. c-.][c()bes] r4 |
	r4 [a8-. c-.][c()bes] r4 |
	r4 [a8-. c-.][c()bes][bes()a] |
	%50
	bes4 r r2 |
	R1 |
	es,4\p r r r8 es(|)as4 r r r8 as (|)g4 r r r8 g(|
	%55
	)d4 r r r8 d(|)es4 r-"cresc." r  r8 es(|)as4 r r r8 as(|
	)g4 r r r8 g(|)d4 r r r8 d(|
	%60
	)es4 r r r8 es(|)as4 r r r8 as | bes2:16 as: | g: e: |
	f4\p r r r8 f(|
	%65
	)bes4 r r r8 bes(|)as4 r r r8 as(|)e4 r r r8 e(|
	)f4-"cresc." r r r8 f(|)bes4 r r r8 bes |
	%70
	c2:\ff bes: | a: fis: | g4\p-"cresc." r r r8 g(|)c,4 r r r8 c |
	[f8\sf(\>as f as][f g d)\!g] |
	%75
	r [es'\p(c)es] r [d(bes)d] | r [c(g)c] r [bes(g)bes] |
	r [as(g)as] r [bes(g)bes] |
	[d,-.\pp d'-.] es4. es8-. d4 ~ |
	[d8 d-.] es4. es8-. d4 ~ |
	%80
	[d8-"cresc." d-.] es4. es8-. d4 ~ |
	[d8 d-.] es4. es8-. d4 ~ |
	[d8 d-.] es4. es8-. d4 ~ |
	[d8 d-.] es4. es8-. d4 |
	fis,2:\ff fis4: g: |
	%85
	d2: d: | fis: fis4: g: | c,: d: c: d: | b2: b4: c: | f': es: f: es: |
	%90
	b2: b4: c: | d: c: d: c: |
	c,4 r8 c' c4\sf()des |
	r r8 c c4\sf()des | 
	r r8 c c4\sf()des | 
	r r8 c c4\sf()des | 
	[c16 es es es]es4:[es16 fis fis fis]fis4: |
	[b,16 d d d]d4:[bes16 e e e]e4: |
	[as,16 c c c ]c4:[g16 c c c]c4: | [a16 c c c]c4:[g16 c c c]c4: |
	%100
	[c8 d,]d4. es8-. es4 ~ | [es8 c-.]c4. d8-. d4 |
	% shared with cello from here on;
	% except for some abbrevs below...
	[g8\p bes cis d][es d bes g]|
	[fis a cis d][es d a fis\f] |
	[g bes cis\p d][es d bes g]
	%105
	[fis a cis d][es d a fis\f] |
	[g bes cis\p d][f, a cis d] |
	[es, g b c][d, g a bes] |
	[c, es g a]r[d, fis g] |
	r[c, es g][d fis a d] |
	%110
	% cello has eighth notes here...
	[g,16 g bes bes][d d es es][f, f a a][cis cis d d] |
	[es,8: g: b: c!:][d,: f: a: bes:] |
	[c,: es: g: a:][cis,: e: g: bes:] |
	[d,: g: bes: d:][d,: fis: a: d:] | d2: d:
	%115
	| d: d: | d: d: | d: d: |
	% urg, 2 copied from 102
	[g,8\p bes cis d][es d bes g]|
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
	% shared with cellos until here
	% shared with violino-2 from here on
	[as as c es][f es c as]|[a c es f][ges f c a]|
	[bes des f ges][as ges des bes]|
	%135
	[c es g as][bes as es c]|[des\p f a bes][c, es g as]|
	[bes, des f ges][as, c e f]|[ges, bes d es][f, as c des]|
	[es, ges bes des][as c es as,]|
	%140
	% shared with cello from here on;
	% except cello has eighth notes
	[des,16\f des f f][a a bes bes][c, c es es][g g as as]|
	[bes8: des: f: ges:][as,: c: e: f:]|
	[g,: bes: c: des:][f,: bes: c: des:]|
	[e,: g: bes: des:][c,: e: g: c:]|
	%shared with cello from here on
	[f,8\p-"cresc." as c des][es, g b c]|[des, f a bes][c, es g as]|
	% uhuh, still 'same as cello';
	% but now rests for notes that viola cannot reach...
	%145
	r[des, e g]r[c, e f]|
	% hmm, don't want to miss more than one note,
	% take bes octave higher
	r[bes des, f][c e g c]|
	% cello has eight notes;
	% viola has rest where can't reach	
	[f,:\ff as: c: des:][es,: g: b: c:]|[des,: f: a: bes:][c,: es: g: as:]|
	%150
	r[des,16 des][e e g g]r8[c,16 c][e e f f]|
	r8[des16 des][f f g g][c, c e e][g g c c]|
	f,1\ff ~ | f | f'4 r r2
	R1
	%155
	f,1 ~ | f | d'2\sf [g,8-. bes-.][bes()as] | 
	es'2\sf [g,8-. bes-.][bes()as] |
	% ugh, should be quoted and transposed...
	%160
	[f8-.\p as-.][as()f][g-. c-.][c()b] |
	R1*2 | 
	g2\ff [es8-. g-.][g()f] |
	g2\sf [es8-. g-.][g()f] |
	%165
	[e-. bes'-.][bes()as][g-. des'-.][des()c] |
	R1*2 |
	r4 [e,8-.\ff g-.][g()f] r4 |
	r4 [e8-. g-.][g()f] r4 |
	%170
	r4 [e8-. g-.][g()f] r4 |
	r4 [e8-. g-.][g()f][fis-.fis-.] | 
	g4 [fis8-.as-.][as()g] r4 |
	r4 [fis8-.as-.][as()g] r4 |
	r4 [fis8-.as-.][as()g] r4 |
	%175
	r4 [fis8-.as-.][as()g] c4\sf(|)b  r r2 | R1
	% ugh, should be quoted and transposed
	c4\p r r r8 c8(|
	)f4 r r r8 f8(|
	%180
	)e4 r r r8 e8(|
	)b'4 r r r8 b8(|
	)c4 r-"cresc." r r8 c,8(|
	% copied
	)f4 r r r8 f8(|
	)e4 r r r8 e8(|
	%185
	)b'4 r r r8 b8(|
	)c4 r r r8 c,8(|
	)f4 r r r8 f8|
	g2:16\ff f: | e: cis: |
	%190
	c,4\p r r r8 c8(|
	)g'4 r r r8 g8(|
	)f4 r r r8 f8(|
	)cis4-"cresc." r r r8 cis8(|
	)d4 r r r8 d8(|
	%195
	)g4 r r r8 g8|
	% check
	[a16\ff a' a a]a4: a2: | fis: dis: | 
	e,4\p r r r8 e8(|
	)a4\< r r r8 a8(|
	%200
	[)g(g e g][e g e)g] |
	% quote and transpose
	[f(g f g][f g f)\!g] |
	[c\f\>(c g)c]r[c(bes)\!c] |
	r[c\p(as)c]r[c(g)c] |
	r[c(f,)c']r[c(g)c] |
	%205
	r[c\p(as)c]r[c(a)c] |
	[b-. g'-.] as4. as8-. g4 ~ |
	[g8 g-.] as4. as8-. g4 ~ |
	[g8 g-.-"cresc."] as4. as8-. g4 ~ |
	[g8 g-.] as4. as8-. g4 ~ |
	%210
	[g8 g-.] as4. as8-. g4 ~ |
	[g8 g-.] as4. as8-. g4 ~ |
	b,2:16\ff b4: c: | g'2: g: | 
	%214
	b,2:16\ff b4: c: | g'2: g: | 
	e2:16\ff e4: f: | b,: c: b: c: | 
	e2:16\ff e4: f: | b,: c: b: c: | 
	%220
	r4 r8 es8 es4\sf()fes |
	r4 r8 es8 es4\sf()fes |
	r4 r8 es8 es4\sf()fes |
	r4 r8 es8 es4\sf()fes |
	[des16 des f f]des4: [b16 b d d]b4: |
	%225
	% check
	[bes16 bes c c]bes4: [a16 a c c]a4: |
	[f16 f b b]f4: [as16 as c c]as4: |
	[as16 as d d]as4: [as16 as c c]as4: |
	[g8 g]g4. as8 as4 ~ |
	[as8 as]f4. f8 g4 |
	%230
	c\p r r2 | r r4 r8 b\f |
	c4 r r2 | r r4 r8 b\f | c4 r r2 |
	R1*3 |
	[c,16c es es][g g as as][bes bes d d][fis fis g g] |
	[as, as c c][e e f f][g, g bes bes][d d es es] |
	%240
	f,4 r r2 |
	R1*11 |
	c'4\p-"cresc." r r r8 c8(|
	% check slur and oct
	)f4 r r r8 f8 | % f,8 (|
	es1 | 
	%255
	ges\f | f\p | as\f | g\p
	e2:16\f e: | 
	%260
	[as,8\p-"cresc." f']f4. es8 es4 ~ |
	[es8 as]as4. g8 g4 ~ |
	[g8 f]f4. es8 es4 ~ |
	[es8 as]as4. g8 g4 |
	r r8 d\ff es4.\sf()d8 |
	%265
	r4 r8 d es4.\sf()d8 |
	r4 r8 e f4.\sf()e8 |
	r4 r8 e f4.\sf()e8 |
	r4 r8 e g4.\sf()f8 |
	r4 r8 f as4.\sf()g8 |
	%270
	g2:16-"sempre" g:\ff | g: g: | g: g: | g: g: |
	[g8 b, c g][c b c g] |
	%275
	[c b c g][c b c g] |
	c,1 ~ | c | f4-. r r2 | R1 |
	c1 ~ | c | f4-. r r2 | R1 |
	c1 ~ | c | fis4-. f-. r2 | R1 |
	es4-. e-. r2 | f4-. g-. r2 |
	%290
	as4-"dim." r g r | R1 | c4\p r r2 | R1 |
	g4-"pizz." r r2 | R1 | c,4 r r2 | R1*15 |
	c4\pp r r2 |
	c4 r r2 |
	c4 r r2 |
}

% urg
\include "viola-2.ly";


% $viola1_staff = \type Staff = viola1 <
$viola1_staff = \type Staff = violai <
	\property Staff.midi_instrument = "viola"
	\property Staff.instrument = "Viola"
	\property Staff.instr = "Vla."
	\clef "alto";
	\notes \type Voice=one < 
		\global
		\$viola1
	>
>

