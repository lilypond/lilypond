\header{
filename =	 "timpani.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.110";

timpani = \notes \relative c {
	R1*2 |
	c4-.\ff r r2 |
	R1*3 |
	c4-. r r2 |
	R1*3 |
	c4-. r r2 |
	R1 |
	g4-. r r2 |
	g4-. r r2 |
	R1*5 |
	r2 r4 g4-. |
	R1*6 |
	r4 c-.\f r2 |
	R1*2 |
	\property Voice.crescendoText = "cresc."
	\property Voice.crescendoSpanner = "dashed-line"
	r2 c4-.\p\< r |
	r2 c4-. r |
	r2 c4-. r |
	c r c r |
	\!c1\ff-\trill |
	c8 r g r g r g r|
	g4 r g r|
	g r c r |
	c r c r|
	c r c r|
	c\sf r r2|
	c4\sf r r2|
	R1*3|
	c4\sf r r2|
	R1*38|
	c2:16\ff c4 g|
	c g c g|
	c2:16 c4 g|
	c g c g|
	g2:16 g4 g|
	g c g c|
	g2:16 g4 g|
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
	R1*3|
	g4\f r r2|
	R1|
	g4\f r r2|
	R1*3|
	g4\f r8 g g4 r8 g|
	g4 r8 g g4 r8 g|
	g4 r8 g g4 r8 g|
	R1|
	g1\trill~|
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
	R1*18|
	r2 r4 c\sf|
	g r r c\sf|
	g r r c\sf|
	g r r c\sf|
	g r r c\sf|
	g r r2|
	R1*35|
	g2:16\ff g4 g|
	g c g c|
	g2:16\ff g4 g|
	g c g c|
	c2:16 c4 c|
	c c c c |
	c2:16 c4 c|
	c c c c |
	R1*8|
	g4 r c r|
	c r g r|
	R1|
	r2 r4 r8 g\f|
	c4 r r2|
	r2 r4 r8 g\f|
	c4 r r2|
	R1*3|
	c4\f r8 c c4 r8 c|
	c4 r8 c c4 r8 c|
	c4 r r2|
	R1*23|
	c4\ff r r2|
	c4 r r2|
	c4 r r2|
	c4 r r2|
	c4 r r2|
	g4 r r2|
	c4-"sempre"\ff r c r|
	g r r2|
	c4 r c r|
	g r r2|
	c4 r c r|
	c r c r|
	c r r2|
	R1|
	c4-. r r2|
	R1*3|
	c4-. r r2|
	R1*3|
	c4-. r r2|
	R1|
	c4-. c-. r2|
	c4-. c-. r2|
	\property Voice.decrescendoText = "dim."
	\property Voice.decrescendoSpanner = "dashed-line"
	c4\> r g r|
	R1|
	\!c4\p r r2|
	R1|
	g4 r r2|
	R1|
	c4\pp r r2|
	R1*18|
}

timpaniStaff = \context Staff = timpani <
	\property Staff.midiInstrument = #"timpani"
	\property Staff.instrument = #"2 Timpani\n(C-G)"
	\property Staff.instr = #"Timp."
	\clef "bass";
	\notes< 
%		\global
		\time 4/4;
		\context Voice=timpani
			\timpani
	>
>

