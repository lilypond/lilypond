
\version "1.3.120";

cornoII =  \notes \relative c {
	R1*2 |
	d''4-.\ff r r2 |
	R1*3 |
	d4-. r r2 |
	R1*3 |
	c4-. r r2 |
	R1 |
	e,4-. r r2 |
	e4-. r r2 |
	R1*5 |
	r2 r4 e4-.\f |
	R1*6 |
	r4 g-.\f r2 |
	R1*2 |
	% `a 2
	% urg, yes this is a-deux, but lily doesn't know about I. / a2 yet.
	\property VoiceCombineVoice.crescendoText = #"cresc."
	\property VoiceCombineVoice.crescendoSpanner = #'dashed-line
	r8 f'\p\< f2 e4 |
	r8 f f2 e4 |
	f4. e8 f4. e8 |
	f4. e8 f4. e8 |
	\!d1\ff ~|
	d4(e d)c|
	d4. r8 c4. r8|
	e,4. r8 e4. r8|
	e'4. r8 d4. r8|
	e,4. r8 d'4. r8|
	c2\sf r|
	c\sf r|
	R1*2|
	e1\sf|
	c,\sf|
	g2. g4\sf|
	g2. g4\sf|
	g2. g4\sf|
	g2. g4\sf|
	g4 r r2|
	R1|
	g1\p~|
	g1~|
	g1~|
	g1~|
	g1~|
	g1~|
	g1~|
	g1~|
	g1~|
	g1|
	g2\ff d''|
	e1|
	R1*20|
	c2.\ff e,4|
	r e r e|
	c'2. e,4|
	r e r e|
	d'2. c4|
	d c d c|
	d2. c4|
	d c d c|
	%a deux|
	r r8 c c2\sf|
	r4 r8 c c2\sf|
	r4 r8 c c2\sf|
	r4 r8 c c2\sf|
	c4 r c r|
	e, r e r|
	r2 c'4 r|
	r2 c4 r|
	R1*4|
	e,4\f r r2|
	R1|
	e4\f r r2|
	R1*3|
	e4\f r8 e e4 r8 e|
	e4 r8 e e4 r8 e|
	e'4 r8 e e4 r8 e|
	e4 r8 e r2|
	e1~|
	e~|
	e4. e8 e4 r8 e|
	e4. e8 e4 r8 e|
	e4\p r r2|
	R1*21|
	d4\f r r2|
	g,4 r r2|
	g4 r8 g g4 r8 g|
	g4 r8 g e'4 r8 e|
	d4\p r r2|
	R1*3|
	d4\ff r c r|
	d r c r|
	e, r d' r|
	d r e r|
	d4 r r2|
	R1|
	g,4-.\ff r r2|
	R1*3|
	d'2\sf r|
	d2\sf r|
	R1*3|
	e2\ff r|
	e2\sf r|
	R1*3|
	%a deux|
	d2.\ff r4|
	d2. r4|
	d2. r4|
	d2. r4|
	e2. r4|
	e2. r4|
	e2. r4|
	e2. r4|
	e4 r r2|
	R1|
	e,1\p~|
	e~|
	e~|
	e~|
	\property VoiceCombineVoice.crescendoText = #"cresc."
	\property VoiceCombineVoice.crescendoSpanner = #'dashed-line
	e\<~|
	e~|
	e~|
	e~|
	e~|
	e|
	\!e2\ff d'|
	R1*17|
	%a deux|
	r4 r8 e,\p e4. e8|
	r4 r8 e e4. e8|
	\property VoiceCombineVoice.crescendoText = #"cresc."
	\property VoiceCombineVoice.crescendoSpanner = #'dashed-line
	r4 r8 e\< e4. e8|
	r4 r8 e e4. e8|
	r4 r8 e e4. e8|
	r4 r8 e e4. e8|
	\!d'2.\ff c4|
	d c d c|
	d2.\ff c4|
	d c d c|
	g2. d'4|
	g, d' g, d'|
	g,2. d'4|
	g, d' g, d'|
	r4 r8 d d4\sf()es|
	r4 r8 d d4\sf()es|
	r4 r8 d d4\sf()es|
	r4 r8 d d4\sf()es|
	d2 d|
	%a deux|
	e d~|
	d1~|
	d|
	%a deux|
	d4 r f r|
	d r e r|
	R1|
	r2 r4 r8 e,\f|
	e4 r r2|
	r2 r4 r8 e\f|
	e4 r r2|
	R1*3|
	%a deux|
	c'4\f r e r|
	d r c r|
	d r r2|
	R1|
	e,1\p~|
	e~|
	e~|
	e~|
	e~|
	e~|
	e~|
	e~|
	e~|
	e~|
	\property VoiceCombineVoice.crescendoText = #"cresc."
	\property VoiceCombineVoice.crescendoSpanner = #'dashed-line
	e\<~|
	e~|
	e|
	%a deux|
	\!c'1\f|
	d\p|
	d\f|
	e\p|
	e\f|
	d4\p r r2  |
	R1*3|
	%a deux; urg: copied corno|
	d\ff||
	d||
	e||
	e||
	f||
	e||
	e4.-"sempre"\ff e8 e4. e8||
	e4. e8 e4. e8||
	e4. e8 e4. e8||
	e4. e8 e4. e8||
	e4 r8 e, e4 r8 e|
	e4 r8 e e4 r8 e|
	R1*2|
	d'4-. r r2|
	R1*3|
	d4-. r r2|
	R1*5|
	%a deux|
	e4-. e-. r2|
	d4-. e-. r2|
	R1*25||
}
