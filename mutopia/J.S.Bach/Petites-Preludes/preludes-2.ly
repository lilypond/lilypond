\header{
filename =	 "preludes-2.ly";
% blz 10
% 
% Six Petits Pr eludes
% Collection Johann Peter Kellner
% ca 1703- 1707
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "public domain";
}

\version "1.1.66";
upper = \context Staff \notes\relative c{
	\context Voice=i
	\property Voice . textStyle = "italic"
	r8\mf c'-1( e-2 g-4  e-2 c-1 bes'-5 g-3 |
	)a-4\< c,-1( f-2 a-4  f c c'-5 a-3 |
	)b-4 g-1( b-2 d-4  b-2 g f'-5 \!d-3 |
	<
		{\stemup )e2-4\f r4 e-5 }
		\context Voice = ii {\stemdown <c2 g > r4 <c g > }
	>
	\stemboth |
	< 
		 { \stemup d2-4 r4 d-4 }
		 \context Voice = ii { \stemdown <c2 a > r4 <c a > }
	>
	\stemboth |
	< 
		 { \stemup d8 }
		 \context Voice = ii { \stemdown <b 8 g > }
	>
	\stemboth 
	g, 8-1\mf( b-2 d-4 
	<
		{ \stemup )b 4 e-5 }
		\context Voice = ii { \stemdown g, 4 b }
	>
	\stemboth |
	<
		{ \stemup c2 r4 < {c-5 ~ c8} {a 4 ~ a 8} > }
		\context Voice=ii { \stemdown c2 r4 e, 4 ~ e 8 }
	>
	\stemboth 
	d,-1\p( fis-2 a-4  fis-2 d-1 c'-5 a-3 |
	)b-4\< g-1( b-2 d-4  b-2 g-1 f'!-5 d-3 |
	)e-4 g,-1( c-2 e-4  c-2 g g'-5 e-3 |
	)fis-4 a,-1( c-2 fis-4  c a a'-5 \!fis-3 |
	<
		{ \stemup )g2-5\f }
		\context Voice = ii { \stemdown <d2 b > }
	>
	\stemboth 
	r4 b4-2( |
	c,8-1 e-2 g c  g-2 e-1 bes'-4 g-2 |
	)a16->-3 g-2 f-1 e-3  d c b !-3 a-2 
	b-3\< g-1 a b  c-1 d e \!f |
	g4-5\ff
	<
		{ \stemup c,2-5 b 4-4 }
		\context Voice=ii { \stemdown g 2-2 d 4-1 }
	>
	\stemboth 
	<
		{ \stemup c1^5 }
		\context Voice=ii { \stemdown e, 1_1}
	>
	\stemboth 
	\bar "|.";
}

lower = \context Staff \notes\relative c{
	\context Voice=i
	<
		{ \stemup c1 ~ | c ~ | c }
		\context Voice=ii { \stemdown c,1 ~ | c ~ | c }
	>
	\stemboth |
	r8 c-5( e-3 g-1  e-3 c-5 c' e,-4 |
	)fis-3 d-5( fis-3 a-1  fis-3 d-5 d' fis,-4 |
	)g2-3 r4 g-2( |
	)a8 a,-5( c-4 e-2  c-4 a-5 g' e-3 |
	)fis2 r4 d-4 |
	g-\mordent-"(\\textsharp)" g, 4 g'-\mordent g, |
	% mordents in brackets...
	g'-\mordent g, g'-\mordent g, |
	g'-\mordent g, g'-\mordent g, |
	g'8 g,-5( b-4 d-2  b-4 g-5 f'!-1 d-3 |
	)e-2 c-4 e-2 g-1  e-3 c-5 c' e,-4 |
	\property Voice . textStyle = "finger"
	f1^"3\\_1" ~ |
	f8 d-3 e-2 c-4 g'4-1 g,^"5\\_2" |
	<c1 c,1>
	\bar "|.";
}

global = \notes{
	\time 4/4;
}

\score{
	\context GrandStaff <
		\context Staff = upper <
			\global
			\upper
		>
		\context Staff = lower <
			\global
			\clef "bass";
			\lower
		>
	>
	\paper{
		\include "preludes-paper.ly";
		gourlay_maxmeasures = 4.0;
	}
	\midi{ \tempo 4 = 100; }
	\header {
		opus = "BWV 939";
		piece = "2";
	}
}

