\header{
filename =	 "violino-2.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.0.7";

violino2 = \notes \relative c {
	c'1\ff ~ | c | <f4-. c'-. as'-.> r r2 | R1 |
	c1 ~ | c | <d4-. b'-. as'-.> r r2 | R1 |
	c1 ~ | c | <fis4-. es'-. c'-.> r r2 | R1 |
	%13
	<g,4-. es'-. c'-.> r r2 | <g4-. d'-. b'-.> r r2 |
	R1 *3 |
	r2 [b8-.\p f'-.] [f()es] |
	[es8-. as-.] as4.-"cresc." g8-. g4 ~ |
	%20
	[g8 fis-.] fis4 ~ fis g-.\f |
	R1*4 | r2
	%25
	[a,8-.\p es'-.][es()des] | [des-.-"cresc." ges-.]ges4. f8-. f4~ |
	[f8 e-.]e4-.\f r2 | R1 
	%29
	%copied... \transpose c \violino-1(29, 39)
	r8 f-.\p f4. es8-. es4~ | [es8-"cresc." d-.]d2.~ |
	[d8 d-.]d2.~ | [d8 d-.] d4. d8-. d4~ | [d8 d8-.] d4. d8-. d4 |
	[f16\ff f f f][f f f f]f2:16 |
	%35
	[f16 f f f][es es es es][des des des des][c c c c] |
	b2:16 c: | d!: es: | e: f: | 
	<g: c,:> <as: c,:> |
	%40
	a2\sf [c,8-. es-.][es()d] |
	a'2\sf [c,8-. es-.][es()d] |
	d2\sf [b8-. d-.][d()c] |
	d2\sf [b8-. d-.][d()c] |
	des2\sf [bes8-. d-.][d()c] |
	%45
	c2\sf [a8-. c-.][c()bes] |
	r4[a8-. c-.][c()bes] r4 |
	r4[a8-. c-.][c()bes] r4 |
	r4[a8-. c-.][c()bes] r4 |
	r4[a8-. c-.][c()bes][bes()a]|
	%50
	bes4 r r2 |
	R1*5 |
	g'2.\p-"cresc."( a4 | g f d ) bes | bes'2.( c4 | bes2 )as |
	%60
	[g16 bes bes bes][bes bes bes bes]bes2: | 
	%copied... \transpose c, violino-1(61,74)
	bes: bes: |
	bes:\ff des: | c: bes: |
	as2.\p( bes4 | 
	%65
	as g e )c | c'2.( des4 | c2 ) bes | 
	[as16-"cresc." c c c][c c c c] c2:16 | c: c: | 
	%70
	c:\ff es: | d: c: | bes2.\p-"cresc."( c4 | bes a fis ) d |
	d'\sf\>( as g )\!f |
	%75
	% nono, only looks like transpose
	r8[c'\p(g)c]r[bes(g)bes] | r[g(es)g]r[g(d)g] |
	r[g(es)g]r[g(e)g]|r8 fis-.\pp fis4. fis8-. fis4~|
	% urg, can't copy: \transpose g \violino-1(75,91)
	% a -> fis
	% bes -> g 
	% c -> a
	% d -> bes
	[fis8 fis-.] fis4. fis8-. fis4~|
	%80
	[fis8 g-.] g4. g8-. g4~|
	[g8 g-.] g4. g8-. g4~|
	[g8 a-.] a4. a8-. a4~|
	[a8 bes-.] bes4. bes8-. bes4|
	% different
	a2:16\ff a4: bes: | c: bes: c: bes: |
	a2:16 a4: bes: | c: bes: c: bes: |
	b2: b4: c: | f: es: f: es: |
	%90
	b2: b4: c: | f: es: f: es: |
	% copied: \transpose c, \violino-1(92,95)
	r8 es as,4\sf~as()g |
	r8 es' as,4\sf~as()g |
	r8 es' as,4\sf~as()g |
	%95
	r8 es' as,4\sf~as()g |
	% transpose viola...
	[es16 as as as]as4:16[fis16 a a a]a4:16 |
	[d,16 g g g]g4:16[e16 g g g]g4:16 |
	[c,16 f f f]f4:16[c16 es es es]es4:16 |
	[c16 a' a a]a4:16[c,16 g' g g]g4:16 |
	%100
	[fis8 d]d4. es8 es4~|
	[es8 c-.]c4. d8-. d4 | 
	g,\p r r2 |
	r2 r4 r8 <d'\f a'> | 
	<d4 bes'> r r2 |
	%105
	r2 r4 r8 <d\f a'> | 
	<d4 bes'> r r r8 a'\p |
	g4 r r r8 f |
	es4 r r r8 d | 
	c4 r8 g fis4 r8 c' | 
	% looks lot like violino-1 110-113
	%110
	bes4\f r8 es d4 r8 a | 
	g4 r8 c bes4 r8 f | 
	es4 r8 a g4 r8 cis |
	<[d16 s> <bes g'> <bes g'> <bes g']> <a4:16 fis':> <a2:16 fis':> |
	<bes: g':> <bes: g':> |
	<bes: g':> <bes: g':> |
	<bes: g':> <bes: g':> |
	<bes: g':> <bes: g':> |
	<bes4\p g'> r r2 | 
	% copied... urg: *same* as violino-1(119,128)
	% URG: or is this R1*10??
	% should hara-kiri like viola-1/2...
        a'4 r8 es' d4 r8 a | 
	%120
	b4 r8 es des4 r8 bes | as!4 r8 es' des4 r8 as | g4 r r2 |
	r4 r8 es' des4 r8 as | g4 r r2 |
	%125
	r4 r8 c bes4 r8 e, | 
	as4 r8 des c4 r8 as | 
	g4 r8 des' c4 r8 g |
	as4 r8 es' des4 r8 as |
	R1*3
	%131
	% copied from viola(131,140)
	[as,8 as c es][f es c as]|[a c es f][ges f c a]|
	[bes des f ges][as ges des bes]|
	%135
	[c es g as][bes as es c]|[des\p f a bes][c, es g as]|
	% [bes, des f ges][as, c e f]|[ges, bes d es][f, as c des]|
	% urg, copied, but nog ges, f, on violino!
	[bes, des f ges][as, c e f]|r [bes, d es]r[as, c des]|
	% [es, ges bes des][as c es as,]|
	r[bes bes des][as c es ges]|
	% copied: \transpose c \violino-1(140,149)
	%140
	f4\f r8 bes as4 r8 es | des4 r8 ges f4 r8 c |
	bes4 r8 e f4 r8 bes, | bes4 r8 g'! es4 r8 bes' |
	as4\p r8 des-"cresc." c4 r8 g | 
	%145
	f4 r8 bes as4 r8 es | des4 r8 g f4 r8 c | bes4 r8 f' e4 r8 bes' |
	as4\ff r8 des c4 r8 g | f4 r8 bes as4 r8 es | 
	% copied somewhat from violino-1(150,168)
	%150
	des4 r8 g f4 r8 c | des4 r8 f' c,4 r8 e' | 
	f,1\ff ~ | f | <f4 des'> r r2 |
	%155
	R1  |
	f1 ~ | f | 
	<d2\sf d'!> [f8-. as-.][as()g] |
	<d2\sf d'> [f8-. as-.][as()g] |
	%160
	R1 |
	[b,8-. es-.][es()d] [d-. g-.]g4-"ten." | R1 |
	<e2\ff des'> [e8-. g-.][g()f] |
	<e2\sf des'> [e8-. g-.][g()f] |
	%165
	R1
	[bes,8-.\p g'-.][g()f][e-. g-.]g4-"ten." | 
	R1 |
	% copied \transpose c' viola(168, 175)
	r4 [e8-.\ff g-.][g()f] r4 |
	r4 [e8-. g-.][g()f] r4 |
	%170
	r4 [e8-. g-.][g()f] r4 |
	r4 [e8-. g-.][g()f][fis-.fis-.] | 
	g4 [fis8-.as-.][as()g] r4 |
	r4 [fis8-.as-.][as()g] r4 |
	r4 [fis8-.as-.][as()g] r4 |
	%175
	r4 [fis8-.as-.][as()g] es'4\sf(|
	% copied violino-1(176,201)
	)d1\> ~ | d4( b g \!)f
	e2.\p( f4 | 
	e d b ) g |
	%180
	g'2.( a4 | g2 )f | 
	e4 g2-"cresc." g4 ~ | 
	g g2 g4 ~ |
	g g2 g4 ~ |
	%185
	g g2 g4 |
	[g16 g g g][g g g g]g2:16 | g: g: | g:\ff bes: | a: g: |
	%190
	f2.\p( g4 | f e cis ) a | a'2.( bes4 | a2 ) g |
	[f16-"cresc." a a a][a a a a]a2:16 | 
	%195
	a2 a | a2:16\ff c: | b: a: | g2.\p( a4 | g\< fis dis ) b |
	%200
	b'1 ~ | \!b 
	% similar violino-1 only until 219
	[c8\f\>g(c)g]r[bes(g)\!bes] |
	r[as\p(f)as]r[g(es)g] |
	r[f(c)f]r[g(es)g] |
	%205
	r[as(d,)as']r[a(es)a] |
	[g-. b-.]b4. b8-. b4 ~ |
	[b8-. b-.]b4. b8-. b4 ~ |
	[b8-. c-.]c4.-"cresc." c8-. c4 ~ |
	[c8-. c-.]c4. c8-. c4 ~ |
	%210
	[c8-. d-.]d4. d8-. d4 ~ |
	[d8-. es-.]es4. es8-. es4 |
	as,2:16\ff as4: g: | f: es: f: es: |
	as2:16\ff as4: g: | f: es: f: es: |
	des'2:16\ff des4: c: | bes: as: bes: as: |
	des2:16\ff des4: c: | bes: as: bes: as: |
	% copied violino-1(220, 223)
	%220
	r8 as des,4\sf ~ des()c |
	r8 as' des,4\sf ~ des()c |
	r8 as' des,4\sf ~ des()c |
	r8 as' des,4\sf ~ des()c |
	% looks lot like viola...
	[f16 f as as]f4:16[d16 d g g]d4:|
	%225
	[c16 c g' g]c,4:16[c16 c f f]c4:|
	[b16 b f' f]b,4:16[c16 c f f]c4:|
	[d16 d f f]d4:16[c16 c f f]c4:|
	[d8 g,] g'4. as8 as4 ~ |
	[as8 as] f4. f8 g4 | 
	%230
	c,4\p r r2 | 
	r2 r4 r8 <g\f d'> | 
	<g4 es'> r r2 |
	r2 r4 r8 <g\f d'> | 
	<g4 es'> r r2 |
	%235
	R1*3 |
	%copied violino-1(238,243)
	es'4\f r8 as g4 r8 d | 
	c4\f r8 f es4 r8 bes | 
	%240
	as4-. des-. r2 | R1*3 |
	%245
	[g,8\p( e' c e][g e c )g~] |
	[g8( d' b d][g d b )g~] |
	[g8( e' c e][g e c )g~] |
	[g8( f' d f][g f d )g,~] |
	[g8( es'! c es][g es c )g~] |
	[g8( d' b d][g d b )g~] |
	%250
	[g8( es' c es][g es c )g~] |
	[g8( f' d f][g f d )g,~] |
	[g8( es' c es][g es c )g~] |
	[g8( d' b d][g d b )g~] |
	[g8( es' c es][g es c )g] |
	%255
	<c2:16\f es:> <c: es:> |
	[as8\p( f' des f][as f des )as] |
	<d2:16\f f:> <d: f:> |
	[bes8\p( g' es g][bes g es )bes] |
	<c2:16\f g':> <c: g':> |
	%copied viola-1(260, 273) 
	%260
	[f8\p-"cresc." f']f4. es8 es4 ~ |
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
	g4. d8 es4. d,8 |
	%275
	es4 r8 b8 c4 r8 b |
	c1 ~ | c | <f4 c' as'-.> r r2 | R1
	c1 ~ | c | <d4 b' as'-.> r r2 | R1
	c1 ~ | c | <c'4-. a'-.> <b-. as'-.> r2 | R1 |
	<c4-. g'-.> <g-. c-.> r2 | as4-. <c,-. e-.> r2 | 
	%290
	<c4 f-"dim."> r g' r | R1 | g4\p r r2 | R1 | b,4-"pizz." r r2 | 
	%295
	R1 | c4 r r2 | R1*15 |
	c4-"pizz." r r2 |
	c4 r r2 |
	c4 r r2 |
}

$violino2_staff = \type Staff = violino2 <
 	% MIDI hoort geeneens verschil tussen een
	% eerste en tweede viool ;-)
	\property Staff.midi_instrument = "violin"
	\property Staff.instrument = "Violino II"
	\property Staff.instr = "Vl. II"
	\notes< 
		\global
		\$violino2
	>
>
