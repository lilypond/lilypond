\header {
  filename =    "";
  title =       "Partita II";
  piece =       "Sinfonia";
  description = "6 Partite per il clavicembalo, Partita II (c-minor)";
  opus =        "BWV 826";
  source =      "Editio Musica Budapest (Urtext)";
  composer =    "Johann Sebastian Bach (1685-1750)";
  enteredby =   "Tom Cato Amundsen";
  copyright =   "Public Domain";
}

\version "1.0.7";

global = \notes {
	\time 4/4;
	\key es;
	\skip 1*7;
	\bar "||";
	\skip 1*22;
	\time 3/4;
	\skip 2.*62;
	\bar "|.";
}
  
hoyreOpp = \notes\relative c''  {
	\stemup
	es4^"Grave adagio" r16 [es d. es32] f4 r16 [f16 f. g32] |
	[as8. as16] [as16 g32 f g16. d32] es4 r16 [g g. as32] |
%3
	bes4 r16 [bes,16 bes. c32] [des8. des16] 
	\type Staff < 
		{ \stemup e4 }
		{ \stemup \property Voice.hshift = 1 [des16 c32 bes c16. g32] }
	>
%4
	f4 \stemboth r16 [f g. as32] \stemup d,4 r16 [d g. es32] |
	c4 ~ [c32 es des c des16. f32] [b,16. c32 b16. c32] d4 ~ |
	d2 ~ [d16. g,32 a16. b32] [c16. <b32 g> <c16. a> <d32 b]> |
	<[es8. c> <es16 c]>
	\type Staff <
		{ \stemup fis4 }
		{ \stemup \property Voice.hshift = 1 [es16 d32 c d16. a32] }
	> g2
}

hoyreNed = \notes\relative c'' {
	\clef violin;
	\stemdown
	<c4 g> r16 [c b. c32] d4 r16 [d d. es32] |
	[f8. f16] f8 r <c4 g> r16 [es16 es. f32] |
	<c4 g' > r16 [g16 g16. as32] [bes8. bes16] bes8 r |
	<c4 as> r16 s16*3 r16 [f, f. g32] g4 |
%5
	r16 [es es. f32] f4 [g16. a32 g16. a32] b4 ~ |
	[b8. d,16] [f8. as16] [g16. f32 es16. d32] [es16. d32 es16. f32] |
	g4 <c8 a> r <d2 b>
}

venstreOpp = \notes\relative c' {
	\clef bass;
	\stemup
	<c4 g es> r <b as f d> r |
	r <b f d> <c g es> r |
	<c g e> r r <c g> |
	<c as f> r r16 <[as d,><as. d,><g32 d]> <g4 d> |
%5
	r16 <[g c,><g. c,><f32 c]> 
	\type Staff <
		{\stemup f4~ f}
		{\stemup c4 d}
	> r16 [f f. g32] |
%6
	[as8. as16]~ [as g32 f g16 d] <es4 c> r |
	r <a es c> <g2 d>
}

venstreNed = \notes\relative c{
	\stemdown
	c4 r c r | r c c r | c r r c | c r r16 [c c. bes32] bes4 |
	r16 [bes bes. as32] as4_"tr" g r16 [d' d. es32] |
	\type Staff <
		{ \stemdown \property Voice.hshift = 1 [f8. f16] [b,8. b16] g4 r | }
		{ \stemdown s4 g}
	>
	r g g2	
}

andanteEn = \notes\relative c''{
	[g16^"andante" c32 d es8~] [es16 d32 c es d c b] [c16 g as f] [g c g f] |
	[es16 c'32 d es d c16] [g' c, as' c,] [b c d b] g16 r16 r8 |
%10
	\stemup [g'32 f es d c16 bes~] [bes des8 c16] 
	[as'32 g f e f8~] [f32 g as g f es d es] |
	[f es d c bes16 as~] [as c8 bes16] 
	[g'32 f es d es8~] [es32 f g f es d c b]
%12
	\stemboth
	[c bes as g f16 es'~] [es d32 c es d c b]
	[c d es f g16 c,~] [c b32 c d c b c] |
	[as'32 g f e f16 des~] [des c32 des es des c des]
	[g f es d es16 c~] [c b32 c d c b c]
%14
	[f c b a b16 d] [f32 bes, as g as16 c] 
	[f32 as, g f g8~] [g32 f' g as g16 f] |
	[g16 b,8 c16~] [c16 bes32 as g f es d] [es g f es f es d es] [d8 c] |
%16
	r8 g'~ [g32 as g f g16 as] [bes16 d es8~] [es32 bes es f g16 es] |
	[des16 c f8~] [f32 c f g as16 f] [es c d8~] [d32 f g as bes as g as] |
%18
	[bes32 as g16~] [g32 f e16~] [e32 d c16~] [c32 g' as bes] 
	[as g f16~] [f32 es des16~] [des32 c bes16] [bes32 f' g as] |
%19
	[g f e f bes f e f] [as f e f g f e f] f4~ [f32 es d es f d es f] |
	[bes,32 d f g as8~] [as32 g f as g f es d] 
	[es bes es f g8~] [g32 f es g f es d c] |
%21
	[c'32( a g )fis es16 d~] [d32 g a bes cis,16 d] ~
	[d32 fis g a c, es d c] [fis d e fis g a bes c] |
%22
	[bes32 a g fis ~ fis16 g~] [g d8 es16~] 
	[es16 b8 c16~] [c32 bes as g as16 c]
%23
	[fis,32 es' d c bes'16. a32] [g8( )fis16.-\prall g32] g4~ 
	[g32 f g as g f es d ] |
	[f32 es d c g'8~] [g32 es f g f es d c] [es d c bes g'8~] 
	[g32 d es f es d c bes] |
%25
	[d c bes a g'8~] [g32 c, d es d c bes a] [c bes a g g'8~] 
	[g32 bes, c d c bes a g] |
	as4~ [as32 as g as bes as g as] es'4~ [es32 c b c d c b c] |
	[fis16 es8 d16] [g16 d8 c16] [a'16 c,8 bes16~] [bes32 d c bes g' bes, a g]
%28
	[bes'8-\fermata~ bes32 a g fis] [g bes a g fis e d c] [bes d c es d g fis e]
	[d c bes a bes d bes g] |
	r16 [e'32 fis fis16.-\prall \[2/3 e64 fis g\]1/1 ] 
	\type Staff <
	 	{	\stemup  
			[bes,32 a g fis g32 bes16.~] [bes16 c32 bes a bes c a] [fis8. g16] }
		{	\stemdown s8 g8 ~ g4 d}
   >
}

andanteTo = \notes\relative c{
%8
	[c8 c'] [f, g] [es c] [es g] |
	[c as] [es f] [g g,]
	\stemdown \translator Staff = treble r16 [f''16 es d] |
%10
	[es8 g] [e c] [f as] [f c] | 
	[d f] [d bes] [es g] [es c] |
	\stemboth \translator Staff = bass [as c] [f as,] [g c] [es g,] | 
	[f as] [g f] [es g] [f es] |
%14
	[d f] [as c,] [b d] [g d] | 
	[es as,] [f g] c,4~ [c16 d32 es f g a b] |
	[c8 c'] [bes as] [g bes] [g es] |
	[as c] [as f] [bes as] [g f] |
	[e g] [c e,] [f as] [f des] |
	[bes g] [c c,] [f16 g32 as bes c d e] [f8 es] |
	[d f] [d bes] [g' bes] [es, g] |
	[a, fis'] [g g,] [d' a'] [c d,] |
%22
	[g a] [bes b] [c d] [es c] |
	[a g] [d' d,] [g a] [b g] |
	[c es] [c a] [bes d] [bes g] |
	[a c] [a fis] [g bes] [g es] |
%26
	[c16 d32 es f g as bes] [c16 c, c'8~] [c32 c, d es f g as bes][c16 c, c'8~]|
	[c c,] [bes bes'] [fis d] [g es] |
	\type Staff <
		{ 	\stemup <g4 e> r <d'2 bes g> |
			cis4~ [cis8. cis16] [d8 c16 bes] 
			\type Staff <	
				{ \stemup [a bes c8] }
				{ \stemdown a4 } >
		| }
		{ 	\slurdown \stemdown cis,4 r d2( | )d1 \slurboth }
	>
}

allegroEn = \notes\relative c''{
%30
	[g8^"allegro" g16 a] [b8 c d es] |
	[f16 es d es] [f8 g as d,] |
	[f b, d g,] [g'16 f es d] |
	es8 c'4 [bes16 as] [g f es d] |
	c4~ [c16 d e f] [g as bes g] |
	[e8 des'~] [des16 g, as bes] [as g f e] |
	[f g as8~] [as16 g f es] [d c d f] |
%37
	[es f g8~] [g16 f es d] [c b c es] |
	[d as' d, c] [d es f g] [ f es d c] |
	[b g' f es] [d f d c] [b d b a] |
	[g a b c] [d c b c] [d es f d] |
	[b c d es] [f as g f] [es d c b] |
	[c d es c] [a8 c] [f, es'] |
%43
	[bes16 c d bes] [g8 bes] [es, d'] |
	[a16 bes c a] [fis8 a] [d, c'] |
	[g16 a bes g] [es g d g] [cis, bes' a g] |
	[fis e d e] [fis8 g a bes] |
	[c16 bes a bes] [c8 d] [es a,] |
	[c fis,] [a d,] [d'16 c bes a]
%49
	[bes a g a] [bes8 c d es] | 
	[f16 es d c] [bes f' c bes] [as f' bes, as] |
	[g16 f es f] [g8 a b c] | 
	[d16 c b a] [g d' as g] [f d' g, f] |
	[es c' g f] [es f g as] [g c g f] | 
	[es c' g f] [es f g as] [g c g f]
%55
	[e bes' g f] [e f g as] [g bes g f] | 
	[e bes' g f] [e g bes des] [c bes as g]|
	[as f' c bes] \stemup [as bes c des] [ c f c bes] | 
	[as f' c bes] \stemboth [as bes c des] [c f c bes] |
	[a es' c bes] \stemup [a bes c des] [c es c bes] |
	[a es' c bes] \stemboth [a c es ges] [f es des c] |
%61
	[des8 bes c des es des] | 
	[c as bes c des c] [bes as bes g as bes] |
	[e,16 d c d] [e8 f g as] | 
	[bes16 as g as] [bes8 c] [des g,] |
	[bes e,] [g c,] [c'16 bes as g]
%67
	[as g f8~] [f16 as g f] [es d c bes] | 
	[g' f es8~] [es16 g f es] [d c b c] |
	[f es d8~] [d16 f es d] [c b a g] | 
	[es' f g8~] [g f16 es] [d a' b c] |
	[d c b c] [d g f es] [d c b a] | 
	g4~ [g16 a b c] [d es f d]
%73
	[b8 as'~] [as16 d, es f] [es d c b] | 
	c4~ [c16 g' des c] [bes es bes as] |
	[g as bes c] [des8 bes] g'4~ | 
	[g8 c,16 bes] [as f' bes, as] [g des' g, f] |
	[e f g as] [bes8 g] [e'.-\prallprall d32 e] |
%78
	[f8 f,16 g] [as8 bes c d] | 
	[es16 d c d] [es8 f g as] |
	[b,16 g' d c] \stemup [b c d es] [d g d c] | 
	[b g' d c] [b c d es] [d g d c] |
	[b f' d c] [b c d es] [d f d c] | 
	[b f' d c] [b d f as] [g f es d] |
%84
	[es f g es] [c g' f es] [d c bes as] | 
	\stemboth [g as bes8~] [bes16 c bes as] [g f e d] | 
	[c g' c, bes] [c d e f] [es g c, bes] |
	[c g' f e] [f c f g] [as bes c d] | [es d c d] [es8 f16 g] [as8 des,] |
	[f b,] [d g,] [g'16 f es d] | 
	\type Staff <
		{ \stemup [es8. c16] c4 ~ [b8.-\prall c16] | c2-\fermata }
		{ \stemdown r8 r16 <a16 es> <g2 d> | <g es> }
	>
}
allegroTo = \notes\relative c'{
	\clef bass;
%30
	\type Staff <
		{ \stemup <b8 g> r r4 r }
		{ \stemdown g,8 r8 r4 r }
	>
	r2. | r2. | 	
	\clef treble; r8 [c16 d] [es8 f] [g as] |
	[bes16 as g as][bes8 c] [des g,]  
	[bes e,][g c,][c'16 bes as g] | [as8 f g as][bes as] | 
	[g es f g as g] | [f es f d es f] | [g g,16 a] [b8 c] [d es] |
	[f16 es d es][f8 g][as d,] |
	[f b,][d g,][g'16 f es d] | \clef bass;
	[es d c8~][c16 es d c][bes a g f] |
%43
	[d' c bes8~][bes16 d c bes][a g fis g] |
	[c bes a8~][a16 c bes a][g fis e d] | [bes'8 d][g, bes][e, cis'] |
	[d8 d,~][d c16 bes][a g fis e] | d4~ [d16 e fis g][a bes c a]
%48
	[fis8 es'~][es16 a, bes c][bes a g fis] |
	g4 r16 [g' as bes][as g f es] | [d8 f][d bes][c d] |
	[es es,] r16 [es' f g][f es d c] | [b8 d][b g][a b] |
	[c c,] r16 [d''16 es f][es8 d] | [c c,] r16 [as' bes c][bes8 as] |
	[g c,] r16 [d' e f][e8 d] | [c c,] r16 [d e f][e8 c]
%57
	[f f,] \stemdown \translator Staff = treble r16 [g'' as bes][as8 g] |
	[f  \stemboth \translator Staff = bass f,] r16 [des'16 es f][es8 des] |
	[c f,] \stemdown \translator Staff = treble r16 [g' a bes][a8 g] | 
	[f \stemdown \translator Staff = bass f,] \stemboth r16 [g a bes][a8 f] |
	[bes16 c des8~][des16 c bes as][g f g bes] |
%62
	[as bes c8~][c16 bes as g][f e f as] | 
	[g des' g, f][g as bes c][bes as g f] | 
	[c'8 bes16 as][g c bes as][g f e d] |
	c4~ [c16 d e f][g as bes g] | 
	[e8 des'~][des16 g, as bes][as g f e] |
%67
	[f g as f][d8 f][bes, as'] | 
	[es16 f g es][c8 es][as, g'] |
	[d16 es f d][b8 d][g, f'] | 
	[c16 d es c][as c g c][fis, es' d c] |
	[b a g a][b8 c d es] | 
	[f16 es d es][f8 g as d,] |
%73
	[f b,][d g,][g'16 f es d] | 
	[es d c d][es8 f g as] |
	[bes16 as g f][es bes' f es][des bes' es, des] |
	[c bes as bes][c8 d e f] | 
	[g16 f e d][c g' des c][bes g' c, bes] |
	[as f' as, g][f c' des g,][a es' f b,]
%79
	[c f es d][c g' as d,][es b' c f,] | 
	[g8 g,] \stemdown \translator Staff = treble r16 [a'' b c][b8 a] |
	[g \translator Staff = bass g,] \translator Staff = treble
		r16 [es' f g][f8 es] | 
	[d \stemboth \translator Staff = bass g,] r16 [a b c][b8 a] |
	[g8 g,] \stemdown \translator Staff = treble r8 [g''16 f][es d c b] |
	[c8 \stemboth \translator Staff = bass c,16 d][es8 f g as] | 
	[bes16 as g as][bes8 c des g,] |
%86
	[bes e,][g c,][c'16 bes as g] | as4~ [as16 bes as g][f g as f] |
	[g f es d][c bes as g][f f' e f] | [as f e f][b, f' es d][es g c, es] |
	[fis,8. fis'16] g4 g, | c,2-\fermata
}

\score {
     \type GrandStaff < 
      \type Staff = treble < 
        \global 
        { \hoyreOpp \andanteEn \allegroEn }
        \hoyreNed
      >
      \type Staff = bass <
        \global
        { \venstreOpp \andanteTo \allegroTo }
	\venstreNed
      >
    >
  \paper {}
}

% EOF



