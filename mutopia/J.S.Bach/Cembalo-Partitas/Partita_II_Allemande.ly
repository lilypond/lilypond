\header{
	enteredby = "Tom Cato Amundsen";
	copyright = "Public Domain";
	piece = "Allemande";
}

\version "1.0.7";

global = \notes{
	\time 4/4;
	\partial 8*3;
	\key es;
	\skip 1*15;
	\skip 4*3;
	\skip 16*4;
	\bar ":|:";
	\skip 16*3;
	\skip 1*16;
	\bar ":|";
}

hoyreEn = \notes\relative c'''{
%	\partial 4.; 
	[as16 g f] |
	[g16 d es b][c f es d][es b c g][as d c b] |
	[c g as es][f es' d c][b8. a16][g8 b] |
	[c16 d es8][f,16 es' d c][d es f8][g,16 f' es d] |
	[es16 f g8][as,16 g' f es][f g as8][b,16  as' g f] |
%5
	\type Staff < { 
		\stemup es4~ [es8. c16] d4~ [d8. b16] |
		c4.~ [c16 b] c4~ [c16 c bes as] |
		\stemboth [g c32 d es16 c][g8 as] \stemup bes4~ [bes8 g'~] |
		\stemboth [g16 c,32 d es16 c][a f' c32 d es16] \stemup d4~ [d8 d] |
		[d8 c] r16 [fis g a][es8 d] r16 [g a bes] |
		[f8 e] r16 [a bes c][g8 fis] r16 [es d c] |
	} {
		\stemdown r16 [b c g][as es f8] r16 [g as f][g d es8~] |
%TODO når du får tak i en til urtekst må denne og neste takt sammenliknes.
		[es16 f32 g as16 f][d f8.~][f16 f es d][es8 f] |
		s4 r4 r16 [f g d] es8 r |
		s2 r16 [a bes8~][bes fis] |
		g4 r a r |
		bes r c r
	} >
%11
	[d16 a bes fis][g c bes a][bes fis g d][es a g fis] |
	[g d es b][c f es d][es c d fis][a es d c] |
	[bes d g bes~][bes fis g bes,][c es g bes~][bes fis g c,] |
	[d fis g bes~][bes fis g d][es fis g bes~][bes fis g bes] |
	[e a, bes fis][g bes a g][fis d es bes][c es d c] |
	\type Staff <
		{ \stemup [bes8 d'][bes a16 g] g4~ g16 }
		{ \stemup s2 s16 d8.~ d16 }
		{ \stemdown r8 <g d> <[g d><fis c]> r16 [d16 b8~] b16 }
	>
	[es'16 d c] |
%17
	[d a b g][as f es d][es( f32 )g c,8] r16 [as'' g f] |
	[g d e c][des bes as g][as( bes32 )c f,8] r16 [c f g] |
	[as e f c][des c' bes as][bes f g d][e des' c bes] |
	[c g as e][f es' des c][des a bes g] g'4~ |
	[g16( )c, f( )e][as( g f )e][f( )c es( )des][bes'( as g )f] |
%22
	[e16( f32 )g c,16 bes][as f' g, e'] f4~ [f32 g f e f16 g] |
	[as16 e f c][as f es' c][d a bes d][f as g f] |
	[g d es bes][g es d' b][c g as c][es g f es] |
	[f c d as][f d c' a][b fis g b][d f es d] |
	[es b c as][g f es' d][c b c d][g,8 d'] |
	[es16 g c g][es d c b][c es g8~][g16 bes, as g] |
%28
	[as c f c][as g f e][f b d8~][d16 f, es d] |
	[c8 es'( as, )g][d f'( as, )g] |
	[es g'( as, )g][f as'16( g][as g fis )g] |
	fis4.~ [fis16 e32 fis][g16 d f es][d c b a] |
	\type Staff <
		{ \stemup [g8 g'][es d16 c] c4. }
		{ \stemdown r8 <g c es> <[g c> <f b]> <es4. g> } 
	>	s8
}

venstreEn = \notes\relative c{
	\clef bass; 
%	\partial 4.; 
	r16 r8 |
	[c8 c,] \clef treble; r16 [as''' g f][g d es b][c f es d] |
	\clef bass; [es b c g][as8 f][g g,~][g16 f' es d] |
	[es b c g][as g' f es][f c d a][bes as' g f] |
	[g d es b][c bes' as g][as es f c][d8 b'] |
%5
	[c8 es][f, as][b, d][es, g] |
	[as f][g g'][c, g']
	\type Staff <
		{ \stemup [c8 d] | es4 }
		{ \stemdown c4 ~ | [c8. g32 f] }
	>
	\stemboth [es, f g16 c,32 d es16][g,8. bes32 as][g as bes16 es,32 f g16] |
%8
	[a,8 c][f a][bes. f''32 es][d es f16 bes,32 c d16] |
	[es, g a bes][c d es c][fis, a bes c][d e fis d] |
	[g, bes c d][e fis g es][a, c d es][d c bes a] |
	[bes c d a][bes es d c][d a bes fis][g c bes a] |
	[bes fis g d][es32 f g16 c,32 d es16] fis,4 r8 d8 |
%13
	[g8 bes'( es, )d][a c'( es, )d] |
	[bes d'( es, )d][c es'( es, )d] |
	<cis4 cis,> r8 cis8 [d a][fis d] |
%16
	[g16 a bes c][d8 d,]
	\type Staff <
		{ \stemup r8 r16 d' g }
		{ \stemdown g,4~ g16 }
	>
	r16 r8 |
%17
	[g,8 g'] r16 [as g f][g d es b][c f, es d] |
	[c8 c'] r16 [des' c bes][c g as e][f bes, as g] |
	[f g as8][bes,16 as' g f][g as bes8][c,16 bes' as g] |
	[as bes c8][des,16 c' bes as][bes c des8][e,16 des' c bes] |
	[as8 c][f, as][des f g, bes~] |
%22
	[bes8 as16 bes][c8 c,][f16 c' f g][as f as c] |
	[f8 as, c f,][bes f' d bes] |
	[es g, bes es,][as es' c as] |
	[d f, as d,][g d'][b g] |
	[c es,][as f][g g,~][g16 as' g f] |
	[g d es b][c f es d][es b c g][as des c bes] |
%28
	[c g as e][f bes as g][as f g b][d as g f] |
	[es( g c )es~][es b c es,][f( as c )es~][es b c f,] |
	[g( b c )es~][es b c g][as b c es~][es b c es] |
	[a d, es b][c es d c][b g as es][f as g f] |
	[es g c es][g8 g,] <c4. c,> s8
}

\score{
	\type GrandStaff <
		\type Staff = treble <
			\global
			\hoyreEn
		>
		\type Staff = bass <
			\global
			\venstreEn
		>
	>
	\paper{}
}