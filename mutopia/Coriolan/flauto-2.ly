
\version "1.3.120";

flautoII =  \notes \relative c {
	R1 *2|
	as'''4-.\ff r r2|
	R1 *3|
	b4-. r r2|
	R1 *3|
	c4-. r r2|
	R1|
	es4-. r r2|
	d,4-. r r2|
	R1 *4|
	r2 r2|
	r2 r4 b'4-.|
	R1*6|
	r4 c r2|
	R1*6|
	c1 ~|
	c4( es, des)c|
	g'4. r8 g4. r8|
	d4. r8 es4. r8|
	c'4. r8 c4. r8|
	c4. r8 c4. r8|
	a2\sf r|
	a\sf r|
	as\sf r|
	as\sf r|
	g1\sf|
	ges\sf|
	f4 bes2.~|
	bes4 bes2.~|
	bes4 bes2.~|
	bes4 bes2.~|
	bes4 r r2|
	R1*33|
	a2.\ff()bes4|
	c(bes c)bes|
	a2.()bes4|
	c(bes c)bes|
	b2.()c4|
	f,(es f)es|
	b'2.()c4|
	f,(es f)es|
	r2 c'4\sf()bes|
	r2 c4\sf()bes|
	r2 c4\sf()bes|
	r2 c4\sf()bes|
	R1|
	r2 g2|
	as2 c,~|
	c1|
	c4 r es r|
	c r d r|
	es,\p r r2|
	r r4 r8 d'\f|
	d4 r r2|
	r r4 r8 d\f|
	d4 r r2|
	R1*3|
	bes4\f r8 es d4 r8 a|
	g4 r8 c bes4 r8 f|
	es4 r8 a g4 r8 bes'|
	bes4 r8 bes a4 r8 a|
	bes1~|
	bes~|
	bes4 r8 bes bes4 r8 bes|
	bes4 r8 bes bes4 r8 bes|
	bes4\p r4 r2|
	R1*21|
	f4\f r r r8 es|
	des4 r r r8 c|
	bes4 r8 bes' bes4 r8 bes|
	bes4 r8 bes bes4 r8 bes|
	R1*4|
	as,4\ff r8 des c4 r8 g|
	f4 r8 bes as4 r8 es|
	des4 r8 g f4 r8 as'|
	g4 r8 g g4 r8 g|
	as4 r r2|
	R1|
	f4\ff r r2|
	R1*3|
	d'2\sf r|
	d2\sf r|
	R1*3|
	des2\ff r|
	des2\sf r|
	R1*35|
	e,1(|
	)d|
	c4\f r r2|
	R1*9|
	d2.\ff()es4|
	b(c b)c|
	d2.\ff()es4|
	b(c b)c|
	g'2.()as4|
	bes( as bes)as|
	g2.()as4|
	bes( as bes)as|
	% 220 a deux?|
	R1*5|
	r2 a|
	b c|
	d c|
	b4 r as r|
	f r g r|
	c,\p r r2|
	r r4 r8 g'\f|
	g4 r r2|
	r r4 r8 g\f|
	g4 r r2|
	R1*3|
	es4\f r d r|
	c r8 f es4 r8 bes|
	as4 des r2|
	R1*14|
	%a deux|
	as'1\f~|
	as\p|
	bes\f~|
	bes\p|
	c1\f~|
	\property VoiceCombineVoice.crescendoText = #"cresc."
	\property VoiceCombineVoice.crescendoSpanner = #'dashed-line
	c\p\<~|
	c~|
	c~|
	c~|
	\!c\ff~|
	c~|
	c~|
	c~|
	c|
	d|
	%a deux urg: copied flauto-1(270,277)|
	es4.-"sempre"\ff b8 c4. as8|
	g4. as8 g4. f'8|
	es4. b8 c4. as8|
	g4. as8 g4. f'8|
	es4 r8 b c4 r8 b|
	c4 r8 b c4 r8 b|
	c4 r r2|
	R1|
	as4-. r r2|
	R1|
	%a deux|
	c,1~|
	c|
	b'4-. r r2|
	R1|
	%a deux|
	c,1~|
	c|
	es4-. d-. r2|
	R1|
	c4-. g'-. r2|
	as4-. e-. r2|
	\property VoiceCombineVoice.decrescendoText = #"dim."
	\property VoiceCombineVoice.decrescendoSpanner = #'dashed-line
	f4\> r f r|
	R1|
	\!es4\p r r2|
	R1|
	b4 r r2|
	R1|
	c4\pp r r2|
	R1*18|
}

