\header{
title =	 "Konzert Nr. 3 Es dur"
subtitle = "f\\\"ur Horn und Orchester";
description =	 "Third horn concerto, for horn in Es";
composer =	 "W. A. Mozart (1756-1792)";
enteredby =	 "HWN";
opus = "K.V. 447";
copyright =	 "public domain";
}


\version "1.0.7";

allegro =
	\notes
	\relative c'
{
	\time 4/4;
	\partial 4;
	R1*4
	c'2^"tutti" g
	c, r
	R1*1
	r8 [g' g g] [g g e c]
	g4 g, r2
	R1*18
	r2 r4 g'4 |
	e'4. () c8 [f (d c )b ]
	[b()c] g4 r8 [g-. c-. e-.]
	g2. ( [g16-.( f e ))f]
	dis4 () e4-- r8 [c8-. c-. c-.]
	c4.( [d16 )e]  f4 () e
	a,() d g,() c
	d-. d-. [d8. c16 d8. e16]
	c4 r r2
	R1*3
	c,2 ~ [c8 e( g )c]
	[c () b ] b4-. r2
	[c,8 () e g c] [e()g e c]
	[c()b] b4-- r2
	c4.() g8 e'4.() c8
	[g'()d ] d4-- r4 d
	[d8 () c] c4.( [d16 e] [d8 )c]
	[c8(-\trill )b] b4 r2 |
	d2~(  [d8 e16 d] [c8 )b] |
	[b()a-.] a4-> r8 [a-. a-. a-.]
	a4 cis e g
	[g16( fis e )d] d4-. r2 |  %  The g16 really is a grace note.
	% mark B
	R1*3
	r2 r4 [d8 (b ]
	[a )g d'( b] [a )g e'( c ]
	[b8 )a] a4 r4 [a8-. a]
	[a( b c cis] d4 )c
	[ais8() b] r8 b [b()c] r c
	[cis ()d] r4 r2
	g,1 ~ g2 ~ [g8 a16 b] [c()d e c]
	f4-. d-. b-. g-.
	r1
	c,2\p e4 g c e g4. e8 |
	d4. [e16 fis] [g () fis e d] [c() b a g]
	a1(-\trill % \grace{g a}
	)g4 r r2
	R1*15
	% mark D
	bes2 d4 f
	g,2~ [g8 g'( es )c]|
	bes4()a. [c8 d es]
	cis4()d r8 [bes (c )d]
	es2 () d4 r
	es2\p () d4 r
	[c8(\mf g' es )c] bes4()c-.
	c4.( cis8 )d4 r
	R1*2
	es1~es1|
	e!
	d
	c
	c,
	e'
	e,
	c'2 [b8( a gis )a]
	[gis8 e gis b ] e4 r |
	r8 [e, a c] dis4 r
	r8 [e, a c] dis4 r
	r8 [e, g b] e4 r
	r8 [fis, b dis] fis4 r
	r8 [gis, b d] f4 r
	r8 [g, b d] f4 r
	%mark E
	R1*8
	r2 r8 [g,-. g-. g-.]
	e'4.() c8 [f( d c )b]
	[b()c] g4 r8 [g c e] |
	g2. ( [g16( f e ))f]
	dis4()e r8 [c-. c c]
	c4.( [d16 e] f4 )e
	a, ( d g, )c
	d d [
	   % \grace {e}
	   d8. c16 d8. e16] 
	c4 r r2
	% mark F
	R1*3
	c,2~[c8( e g )c]
	[c8()b] b4 r2
	[c,8()e g c ] [e ()g e c]
	[c()b] b4 r2
	c2 (bes )a [a8(b c )cis]
	d2( ~ [d8 e16 d] [d16() c b )c]
	[c( b a) g] g4 r2 |
	R1*3
	r2 r4 [g'8()e]
	[d()c g'()e] [d()c a'()f]
	[e()d] d4 r [d8 d]
	d4~( [d16 e d )e] [g8() f e d] |
	c4 r r2
	r1
	c1 ~
	c |
	[c8-. c-.] r c-. [cis()d] r d-. |
	[dis()e] r e-. [e()f] r f-. |
	g4-. e-. c-. bes-. |
	g-.\ff e-. c-. r |
	a'2 ~ [a8 b16 c] [d e d e]
	f4. () d8 [f8 ()d f d]
	[c (e] )g2 [f16 e d c]
	d1-\trill  % \grace { c d}
	c4 r r2
	r1 |
	% mark H
	[/3 c8 ()b a ]/1	[/3 g a b]/1 [/3 c d e]/1 [/3 f()e d]/1 |
	[/3 c () b a ]/1 [/3 g a b]/1 [/3 c d e]/1 [/3 f()e d]/1|
	c4 \[/3 r8 [g'()e]\]/1 c4 \[/3 r8 [e () c]\]/1 |
	g4 \[/3 r8 [c8() g] \]/1 [/3 e ()g e]/1 [/3c ()e c]/1|
	g4 r8 g'\f [a b c d]|
	d1(-\trill % \grace { c d }
	)c4 r r2
	R1*3
	c1-\trill ( %\grace{c d}
	)c4 r r2
	R1*2
	
	r4 [c8.^"tutti" c16] c4 c
	c [c,8. c16] c4 c|
	c2 r2 \bar "|.";
	
}

romanze = \notes \relative c' {

	\key F;
	\time 2/2;
	c'4.() f8 a,4 a
	[bes8( c d bes] )g4 r8 g
	a r bes r c r [d()bes]
	a2()[g8 a( bes )b]
	c4. () f8 a,4 a |
	[bes8 (c d) bes] g4 r8 c,-.
	[c8( e g )bes] [a( c f ) d]
	c r e r f r r4
	% mark A
	R1*8
	g4. f8  [e d c bes]
	[bes( a d )c] c4 r
	R1*2
	g'4. f8 [e d c bes]
	[bes (a d ) c] c4 r
	r1
	[c16\mf () d c-. d-.] [e () f e-. f-.] [g()e c-. c-.] [f()d b-. b-.]
	[c16\p () d c-. d-.] [e () f e-. f-.] [g()e c-. c-.] [f()d b-. b-.]
	[c8 c, c c] [c c c c]
	% mark B
	c1\f
	R1*9
	f'4.(\p )d8 b4 r8 g
	g'4.() e8 c4 r8 cis |
	d4(~ [d16 e d )e] [f8 () d f() d]
	c2()b4 r
	R1*4
	e4. ()g8 c,4 ()cis
	[d8( e f )d] b4 r8 g
	[c ()e g g] [g( f e )d]
	c4( % \grace { e}
	[d8. )c16] [c8 c--( c-- )c--]
	% mark C
	des1\sf %\sfp
	g,1\sf %\sfp
	c\sf   %\sfp
	c,\sf  %\sfp
	R1*3
	r8 [c c c] c2~
	[c8 c' c c] c2~
	[c8 e( g f] [e d c bes]
	% mark D
	)a4 r r2
	R1*3
	c4. () f8 a,4 a |
	[bes8 (c d) bes] g4 r8 c,-.
	[c8( e g )bes] [a( c f ) d]
	c r e r f4 r4
	R1*3
	r2 r4 r8 c,8
	[c8( e g )bes] [a( c f ) d]
	c r e r f4 r4	
	g,1\pp
	c,2~c4. c8
	[c8( e g )bes] [a( c f ) d]
	c r e r f4 r4
	r1
	c8-. r e-. r f4 r4
	c8-. r c,-. r c4 r4|
	 \bar "|.";
}

rondotheme = \notes \relative c' {
	[c'8 c c] [c c c]
	c4( cis8 )d r g,
	[d'8 d d] [d d d]
	d4( dis8 )e r c |
	[c()d e] [f g a]
	[g ()e c] c4 d8
	e4()d8 e4()f8
	e4.()d8 r r |
}

lipbreaker = \notes \relative c'
{
 	r8 [g'-. g-.] [c()e g,-.]
	[c()e g,-.] [c()e g,-.]
	[c c, c] [c c c]
	[c c c] [c c c]
}

rondo = \notes 	\relative c'
{
	\partial 8;
	g'8 |
	\time 6/8;
	\grouping 8*3 8*3;
	\rondotheme
	
	R2.*13 |
	r8 r-\fermata d [d e f]
	[g ()e c-.] [d()e d]
	c4 c8 [d e f]
	[g()e c-.] [d()e d-.]
	c4 r8 r4 r8 |
	R2.*7
	% mark A
	c4.\p [d8 c d]
	c4 r8 r4 r8
	e4. [f8 e f]
	e4 r8 r4 r8
	g4. e4 c8
	g2.~
	[g8 a b] [c d e ]
	e4.()d8 r r
	R2.*4
	e2.~ |
	[e8 d c] [c b a]
	d2.~
	[d8 c b] [b a g]
	g'4()e8 b4()cis8
	%mark B
	d4 r8 r4 r8
	R2.*3 |
	r8 [d-. d-.] [d()g d---.]
	[d()g d-.] [d d d]
	[d()g] r r4 r8
	R2.*1
	\lipbreaker
	c4 r8 [c' d e]
	d4()g8 [c, d e]
	d4 r8 r4 r8
	R2. |
	r4 r8 [c-. d-. e-.]
	d4()g8 [c, d e]
	[d()g fis] [e d c]
	[b () e d] [c b a]
	% mark C
	g4 r8 r4 r8
	r2. |
	%
	r8 [g g] [g( )b b]
	[b()d d-.] [d()g g-.]
	g2.~
	[g8 a g] [f e d]
	\rondotheme
	R2.*12
	r4 r8 r4 c8
	% mark D
	c4 f8 c4 a8
	a4.~a4 a8
	bes4 c8 d4 bes8
	g4. ~ g8 r r
	R2.*3
	r4 r8 r4 c8
	a4. c
	f ~ [f8. e16( d )c]
	bes4 g8 e4 g8
	c4. ~ c8 r r
	R2.*3| 
	r4 r8 r4 c8
	b4()c8 b4()c8
	bes4. ~ bes4 g8
	a4 c8 [f () d b]
	d4. () c8 r r
	% mark E
	R2.*9  |
	\lipbreaker 
	[c8 c' c] c4.~
	[c8 c d] [e e fis] 
	g4 r8 r4 r8
	r2.
	r8 [g g] [g g g] |
	es4. ~ [es8 d c]
	b4 r8 r4 r8
	r2. |
	r8 [g g] [g g g]
	es4.\f~ [es8 d c]
	b4. c4. d4. e4.
	% mark F
	f2.\f ~ |
	f4 r8 r4 r8
	r8 [g\> g] [g g g]
	[fis  g gis] % Edition breitkopf says as (silly!)
		 [a bes \! b]
	 \rondotheme
	R2.*7
	% mark G
	R2.*4
	c,4.\mf c4 c8
	c4. e4 c8
	g'4. g4 g8
	g4. g,4 g8
	c4 r8 r4 r8
	r4 r8 r4 g'8
	[c ()e g,-.] 	[c ()e g,-.]
	[c ()e g,-.] 	[c ()e g,-.]
	% mark H
	g'2._"cresc." bes,2.
	a4. [b16 c d e f g]
	a4. f4 d8
	[c8\f g' e] [c g e]
	[c e' c] [g e c]
	g4 r8 [g''8 e c]
	d2.(-\trill % \grace { c d }
	)c4 r8 r4 r8
	R2.*5
	r8 r8-\fermata d8 [d\p e f]
	[g ()e c] [d()e d]
	[ c c c] [d e f]
	[g()e c] [d()e d]
	c4\f r8 r4 r8
	R2.*5
	[c8\f c, c] [c c c]
	c4 r8 c4 r8
	c4 r8 r4 r8
}

 \include "part-paper.ly"

\score
{
	\allegro
	\paper{ }
	\midi{ \tempo 4=90; }
}

\score
{
	{ 	\property Score.SkipBars = 1
		\romanze
	}
		\paper{ \tempo 4 = 70; }	
	\midi{}
}
\score
{
	\rondo

	\paper{
		\tempo 4 = 100;
	}
	\midi{}
}
