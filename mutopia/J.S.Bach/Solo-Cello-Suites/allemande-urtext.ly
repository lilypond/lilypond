% allemande-urtext.ly
% belongs together with -cello.ly and -viola.ly
% (who is going to make a -violin.ly?)

%{
Well, there are still some scripts in this "urtext".
But merging melodic and scripts doen't work too well yet (see viola_scripts).
%}

n = { \slurnormal }
d = { \slurdotted }
comma = "\\sethuge\\ \\ ,"

allemande_a = \notes \relative c {
	\voiceone a'16 | 
	[a8 bes16()a] \onevoice
	[g()f e()d][d()cis d()e][a,8 \d bes16()g] |
	[f( a )d f,] [e8 cis']\voiceone[d8. e16]\onevoice[f g a bes!] |
	%3
	\voiceone[c8 \n d16()es]\onevoice[d()c bes()a][c()bes a()g][d'8. f,16] |
	[e( g )bes d][c()bes a()g]\n[bes()a g()f]
	<[f8. a> <f16 a]> |
	% urg fix
	[b \n f( e )d][e \d cis'( d )cis]\voiceone[d8.( )e16]\onevoice
	\n[f( e d)e] |
	%6
	[d( c b )c][b( a gis)a][gis8-\prall fis16()e][e' c( a )g] |
	\voiceone [e' a, f d] [d f d b]\onevoice
	[gis( b e )gis] \d [b( d c )b] |
	%8
	\n[c( a f )e][d( f e )d][gis8.-\trill()a16]\d[b d e,()d] |
	% urg
	[c( e a)d]\voiceone [b8.-\trill a16] a8 s4 s8 \onevoice |
	%10
	[d,!16 gis( a b a gis fis )e] \voiceone [e8 f16()e]\onevoice
	[d()c b()a] |
	\voiceone[d8 e16()f]\onevoice\d[e()d c()b]
	\voiceone[d'16()b c()a] \onevoice [e8 gis] |
	[a,8. cis16][e g!( f! )e][f( a )d gis,] a8.
	%13
	e16 \voiceone [e8. f16] \onevoice
	\n[g!()e f()a]\n[cis,( d )e bes][a8.-\trill()g16] |
	[f a'(f)d][g \n b,()cis a'][g(f e)d][fis d()es c(] |
	%15
	[)bes g'(a, )g][fis a d c'][bes(fis)g bes][d()a bes()g] |
	[es(d)es g][c()a bes()g]\d[d(c)d g]\n[bes()fis g()es] |
	%17
	\n[c(bes)c bes'][a(c)es g,]\voiceone[fis8-\trill\d g16()a]\onevoice
	[d,8 es16()c] |
	[bes(d )g bes,][d,8 fis']\voiceone[g8. a16]\onevoice[bes d g, f] |
	%19
	\voiceone[e8.-\trill()f16]\onevoice [g()e c()bes]
	\n[a()f' g,()f][e g'( a )bes] |
	% :-(
	% [bes( a g )f^\comma]
	[bes( a g )f^"\\sethuge{\\ \\ '}"]
	[a()e f()d][bes d(f)a][d()a bes()g] |
	%21
	[a,(\n)g'(\n cis())d][e()g, a()e][f()d bes()d]\d[gis, f'(e)d] |
	% \n[d( cis b )a^\comma][c(a)fis d'][c a( b )d]
	\n[d( cis b )a^"\\sethuge\\ \\ ,"]
	[c(a)fis d'][c a( b )d]
	[f!(d )gis, d'] |
	%23
	[cis(e g!)bes][e()a, bes()g][f()cis d()gis,][a8 cis!] |
	% knee
	\stemup [d,16 \stemboth d''(c!)a][bes!(g)e cis'][d a f d] d,8.
}

allemande_b = \notes \relative c {
	\voicetwo
	s16 |
	% <d'8 g' f'> s8 s2. |
	<f8 g, d> s8 s2. |
	s2 <a,8. d,> s16 s4 |
	<fis'8 a, d,> s8 s2. |
	s1 | 
	%5
	s2 d8. s16 s4 |
	s1 |
	f!16 s16*3 b, s16*3 s2 |
	s1 |
	s4 e [dis8 ~ dis32 e( fis gis][a b c d! c b c )a] |
	%10
	s2 <a8 c,> s8 s4 |
	<b8 gis,> s8 s4 <e,8 a,> s8 s4 |
	s1 |
	<cis'8 g,> s8 s2. |
	%15
	s1*3 |
	s2 c,8 s8 s4 |
	s2 g8. s16 s4 |
	bes8. 
}

allemande = \type Staff \notes<
	\$allemande_a
	\$allemande_b
>

