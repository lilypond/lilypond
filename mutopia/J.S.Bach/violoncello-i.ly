\header{
filename =	 "violoncello.ly";
title =	 	 "Vier Duette";
description =	 "Four duets for Violino and Violoncello (Viola)";
opus =           "BWV";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "Public Domain";
}

$violoncello_i = \notes\relative c{
	[g8()a d,][g b()g][d()fis a][d a()fis]|
	[g()d b'][g b()c][b()e fis][g d()d,] |
	g r r [d'16(b a )g] e'8 d r r [c16(b a )b]g'8 |
	fis r r [g16 fis( e d b' g][)a fis( e d g)e][g fis( e d e)cis] |
	[d8 d'()a][b16 c(d)b g b][c,8 c'()g][a16 b(c)a fis a] |
	[b,8()b' fis][g a()e][fis g()d][e fis()d] |
	g r r [g,16(fis e)d] b'8 a r r [g16(fis e )d]c'8 |
	b r r [d16 b(a)g e' c][d b(a)g c a][c b(a)g a fis] |
	[g d( e fis g a][)b d(c b c )a][b g(a b c b][)e g( fis e d cis] |
	)a'16
}

\include "global-i.ly"

$violoncello_i_staff = \context Staff = violoncello <
	\$violoncello_i
	\clef bass;
	\$global_i
>
\version "1.0.20";
