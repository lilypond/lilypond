\header{
filename =	 "violino-i.ly";
title =	 	 "Vier Duette";
description =	 "Four duets for Violino and Violoncello (Viola)";
opus =           "BWV";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "Public Domain";
}

$violino_i = \notes\relative c'' {
	r4 r8 [g16(fis e)d] b'8 a r r [g16(fis e)d] c'8 |
	b r r [d16(b)a g(e')c][d(b)a g(c)a][c(b)a g(a)fis] |
	[g d(e fis g a][)b d(c b c a][)b g(a b c d][)e g()fis e(d)cis] |
	[a'd,(e fis g a][)b a(g fis g e][)fis a(g fis e )d]
	% [cis d e fis <g8 a,\grace d,\grace>] ~ |
	[cis d e fis <g8 a, d,>] ~ |
	%[<g16 a,\grace d,\grace> e(fis)d cis d]
	[<g16 a, d,> e(fis)d cis d] 
	g,8 <f'4 ~ g, g,> [f16 d(e)c b c]
	fis,8 <e'4 f,> ~ |
	[<e16 f,> c(d)b a b][e, b'(c)a g a][d, a'(b)g fis g][c, g'(a)fis e fis] |
	[b,(d)e fis(g)a][b(d)c b(a)g][fis(g)a b(c)d][e(a)g fis e d] |
	[g d(e)fis g a][b g(fis)g c, a'][b, g'(fis)g a, c]
	[fis, g(a)b <c8( d, a>] ~
	[<c d, a> )b] r [d16(b a)g] e'8 d r r [c16(b a)g] g'8 | <fis a, d,>
}

\include "global-i.ly"

$violino_i_staff = \context Staff = violino <
	<
		%urg
		% \notes\property Voice.textStyle = "large" s4^"Moderato"
		% \notes {s4. \property Voice.textStyle = "large" s4^"Moderato"}
		\$violino_i
	>
	\$global_i
>
\version "1.0.21";
