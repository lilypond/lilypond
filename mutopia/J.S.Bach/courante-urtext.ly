% courante-urtext.ly
% belongs together with -cello.ly and -alto.ly
% (who is going to make a -violin.ly?)

%{
Well, there are still some scripts in this "urtext".
But merging music and scripts doen't work too well yet (see alto_scripts).
%}

\version "0.1.14";
n = { \slurnormal }
d = { \slurdotted }

% write as two voices?
courante = \melodic{
	d'16 |
	[d a f a] ['d f g a]\d[bes()a bes()g] |
	<\voiceone g4 a {\voicetwo \n cis, ~ [cis16 d e f]}>
	\onevoice \d[g()f g()e] |
	[f()d e()c!]\n[bes(a)bes a'][g f e d] |
	%4
	% [cis e \n a,(\d(b][[cis d e f][)g )bes-- a e] |
	[cis e \n a,(b][[cis d e f][)g bes-- a e] |
	% [f a \n'd(\d( e][f g a bes][)c )bes-- d c] |
	[f a \n'd( e][f g a bes][)c bes-- d c] |
	\n<''g4 {f'~ [f16} {c'4 ~ [c16 bes a g]}> [f()es d()es] |
	%7
	[d bes(a)bes][d bes e'! bes,][f' bes, g' bes,] |
	\d['e( g )c' d][e f g a][bes()a bes()g]|
	\n[a f(e)f][a f bes f,][c' f, d' f,] |
	%10
	['cis( e )a' b][cis d e f][g()f g()e] |
	[''d( f' )d' e]\n[d()c d()c][b()a b()a] |
	\n[gis( a )b e,]\n[f()e f()d][e()c d()b] |
	%13
	\n[c a'( b c][b a gis)a]['d a' e, a'] |
	[f a( b c][b a gis)a][f a e, a'] |
	['dis a'( b c][b a gis)a]['e a' e, g] |
	[a e, cis e]['a cis e gis] a8.
	%17
	e'16 |
	[e cis a cis]['e f g a][bes()g cis'()'g] |
	<\voicetwo f4 {\voiceone d' ~ \stemdown[d16 a d e] }>
	[f d a, c!] |
	\n[b f,(d)f]\d['g d'(f)g][b f, d' f,] |
	%20
	[e g c,(d][e f g a][)bes! d c g,] |
	<\voiceone a {\voicetwo [f c'(bes a][g f es d]
	[)es c' f, es]}> |
	<\voicetwo bes,4 {\voiceone d ~ [d16 e! f g]}>
	\onevoice
	[a bes c d] |
	%23
	['e bes,(a)g][a f' \n'g()f]\d['c()g f' e] |
	[f a( bes a][g f e )f][g e f d]|
	[cis g' b, g']['a g' b, g']['cis g' a, g']|
	%26
	[f d f a][d a, d' e][f a, f d]|
	['g d'(g)a][bes g' a, f']['g e' f, d']|
	\n[cis(d)e cis][a cis bes cis][a cis g, cis']|
	%29
	\n['f d'(e f][e d cis)d]['g d' a d]|
	\d[bes d(e f][e d cis)d][bes d a d]|
	['gis d,(e f][e d cis)d][a d a cis]|
	[d a f a]['d f a, d]
	d,8.
}

