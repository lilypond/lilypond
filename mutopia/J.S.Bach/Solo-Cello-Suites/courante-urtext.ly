% courante-urtext.ly
% belongs together with -cello.ly and -viola.ly
% (who is going to make a -violin.ly?)

%{
Well, there are still some scripts in this "urtext".
But merging melodic and scripts doen't work too well yet (see viola_scripts).
%}

n = { \slurnormal }
d = { \slurdotted }

courante_a = \notes \relative c {
	d'16 |
	[d a f a] [d, f g a]\d[bes()a bes()g] |
	\voiceone <g4 a> s4 \onevoice \d[g16()f g()e] |
	% urg, a good case for mean-distance-beam-dir-algorithm!
	[f()d e()c!]\n[bes(a)bes a'][g f e d] |
	%4
	% [cis e \n a,(\d(b][[cis d e f][)g )bes-- a e] |
	[cis e \n a,(b][[cis d e f][)g bes-- a e] |
	% [f a \n d,(\d( e][f g a bes][)c )bes-- d c] |
	[f a \n d,( e][f g a bes][)c bes-- d c] |
	\n\voiceone <c4~ f,> <[c16 f,> bes a g] \onevoice [f()es d()es] |
	%7
	[d bes(a)bes][d bes e! bes][f' bes, g' bes,] |
	\d[e,( g )c d][e f g a][bes()a bes()g]|
	\n[a f(e)f][a f bes f][c' f, d' f,] |
	%10
	[cis( e )a b][cis d e f][g()f g()e] |
	[d,( a' )f' e]\n[d()c d()c][b()a b()a] |
	\n[gis( a )b e,]\n[f()e f()d][e()c d()b] |
	%13
	\n[c a'( b c][b a gis)a][d, a' e a] |
	[f a( b c][b a gis)a][f a e a] |
	[dis, a'( b c][b a gis)a][e a e gis] |
	[a e cis e][a, cis e gis] a8. e'16 |
	%17
	[e cis a cis][e, f g a][bes()g cis()g] |
	\voiceone d'4 ~ [d16 a d e][f d a c!]\onevoice |
	\n[b16 f(d)f]\d[g, d'(f)g][b f d' f,] |
	%20
	[e g c,(d][e f g a][)bes! d c g] |
	\voiceone a s16*3 s4 s4
	d, ~ [d16 e! f g] \onevoice [a bes c d] |
	%23
	[e, bes(a)g][a f' \n g,()f]\d[c()g' f' e] |
	[f a( bes a][g f e )f][g e f d]|
	[cis g' b, g'][a, g' b, g'][cis, g' a, g']|
	%26
	[f d f a][d a d e][f a, f d]|
	[g, d'(g)a][bes g' a, f'][g, e' f, d']|
	\n[cis(d)e cis][a cis bes cis][a cis g cis]|
	%29
	\n[f, d'(e f][e d cis)d][g, d' a d]|
	\d[bes d(e f][e d cis)d][bes d a d]|
	[gis, d'(e f][e d cis)d][a d a cis]|
	[d a f a][d, f a, d]
	d,8.
}

courante_b = \notes \relative c {
	\voicetwo
	\stemdown
	s16 |
	s2. |
	%2
	\n cis4 ~ [cis16 d e f] s4 |
	s2.*3 |
	%6
	{ a,4 s2 }
	s2.*11
	%18
	f'4 s s |
	s2.*2
	%21
	[f16 c'(bes a][g f es d] [)es c' f, es] |
	bes4 s s
}

courante =  \type Staff \notes<
 \$courante_a
 \$courante_b
>

