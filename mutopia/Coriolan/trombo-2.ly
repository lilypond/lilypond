\header{
filename = 	 "trombo-2.ly";
%title = 	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description = 	 "";
composer = 	 "Ludwig van Beethoven (1770-1827)";
enteredby = 	 "JCN";
copyright = 	 "public domain";
}

\version "1.3.120";

tromboII =  \notes \relative c {
	R1*2 |
	c'4-.\ff r r2 |
	R1*3 |
	c4-. r r2 |
	R1*3 |
	c4-. r r2 |
	R1 |
	g4-. r r2 |
	g4-. r r2 |
	R1*5 |
	r2 r4 g4-.\f |
	R1*6 |
	r4 c-.\f r2 |
	R1*2 |
	\property VoiceCombineVoice.crescendoText = "cresc."
	\property VoiceCombineVoice.crescendoSpanner = "dashed-line"
	r2 c4\p\< r |
	r2 c4 r |
	r2 c4 r |
	c r c r |
	\!c1\ff ~ |
	c8 r g r g r g r|
	g4. r8 g4. r8|
	g4. r8 c4. r8|
	c4. r8 c4. r8 |
	c4. r8 c4. r8 |
	c2\sf r|
	c2\sf r|
	R1*3|
	c1\sf|
	R1*38|
	c2.\ff g4|
	c g c g|
	c2. g4|
	c g c g|
	g2. g4|
	g c g c|
	g2. g4|
	g c g c|
	c r r2|
	c4 r r2|
	c4 r r2|
	c4 r r2|
	c4 r c r|
	g r g r|
	c r c r|
	c r c r|
	c r r2|
	r2 d'4 r|
	R1|
	r2 r4 r8 d\f|
	g,,4 r r2|
	r2 r4 r8 d''8\f|
	g,,4 r r2|
	R1*3|
	%a deux|
	g'4\f r8 g g4 r8 g|
	g4 r8 g g4 r8 g|
	g,4 r8 g g4 r8 g|
	d''4 r8 d d4 r8 d|
	g,,1~|
	g|
	g4 r8 g g4 r8 g|
	g4 r8 g g4 r8 g|
	g4\p r r2|
	R1*24|
	c4\f r8 c c4 r8 c|
	R1*4|
	c4\ff r8 c c4 r8 c|
	c4 r8 c c4 r8 c|
	c4 r8 c c4 r8 c|
	r2 c4 r8 c|
	c4 r r2|
	R1*5|
	d'2\sf r|
	d\sf r|
	R1*11|
	r2 r4 c,\sf|
	g2. c4\sf|
	g2. c4\sf|
	g2. c4\sf|
	g2. c4\sf|
	g4 r r2|
	R1*35|
	g2.\ff g4|
	g c g c|
	g2.\ff g4|
	g c g c|
	c2. c4|
	c c c c|
	c2. c4|
	c c c c|
	R1*8|
	g4 r c r|
	c r g r|
	R1|
	r2 r4 r8 g\f|
	c4 r r2|
	r r4 r8 g\f|
	c4 r r2|
	R1*3|
	c4\f r8 c c4 r8 c|
	c4 r8 c c4 r8 c|
	c4 r r2|
	R1*18|
	c1\f~|
	\property VoiceCombineVoice.crescendoText = "cresc."
	\property VoiceCombineVoice.crescendoSpanner = "dashed-line"
	c\p\< ~|
	c~|
	c~|
	c~|
	\!c4.\ff c8 c2~|
	c4. c8 c2~|
	c4. c8 c2~|
	c4. c8 c2~|
	c4. c8 c2|
	r4 r8 g'8 g2|
	c,4-"sempre"\ff r c r||
	g r r2|
	c4 r c r|
	g r r2|
	c4 r c r|
	c r c r|
	%a deux|
	c1~|
	c|
	c4-. r r2|
	R1|
	c1~|
	c|
	c4-. r r2|
	R1|
	c1~|
	c|
	c4-. d'-. r2|
	R1|
	c,4-. c-. r2|
	c4-. c-. r2|
	\property VoiceCombineVoice.decrescendoText = "dim."
	\property VoiceCombineVoice.decrescendoSpanner = "dashed-line"
	c4\> r g r|
	R1|
	\!g4\p r r2|
	R1|
	g4 r r2|
	R1|
	c4\pp r r2|
	R1*18|
}
