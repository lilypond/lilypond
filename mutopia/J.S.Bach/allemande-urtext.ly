% allemande-urtext.ly
% belongs together with -cello.ly and -alto.ly
% (who is going to make a -violin.ly?)

%{
Well, there are still some scripts in this "urtext".
But merging music and scripts doen't work too well yet (see alto_scripts).
%}

\version "0.1.14";
n = { \slurnormal }
d = { \slurdotted }
comma = "\\sethuge,";

% write as two voices?
allemande = \melodic{
	a,16 |
	<[''d8 g' f' a> bes16()a][g()f e()d][d()cis d()e]['g8 \d bes16()g] |
	[f( a )d f,] [e8 cis'] <[''d8. a' d> e16] [f g a bes!] |
	%3
	<['d8 a' fis' c,> \n d16()es][d()c bes()a][c()bes a()g][d'8. f,16] |
	[e( g )bes d][c()bes a()g]\n[bes()a g()f]<[f8. a> <f16 a]> |
	% urg fix
	[b \n f,( e )d][e \d cis'( d )cis]<'d8. {[d'( )e16]}>\n[f( e d)e] |
	%6
	[d( c b )c][b( a gis)a][g8-\prall fis16()e][e' c( a )g] |
	<[f! e'> a, f d]<\voiceone d {\voicetwo[b f' d b]}>\onevoice
	[fis( b e )gis] \d [b( d c )b] |
	%8
	\n[c( a f )d][d( f e )d][gis'8.-\trill()a16]\d[b d e,()d] |
	% urg
	[c( e a')d']< {\voiceone [b8.-\trill a16] a8 } 
	{\voicetwo e,4 ['dis8 ~ dis32 e( fis gis][a b c d c b c )a]}> |
	\onevoice
	%10
	[d'!16 gis'( a b a gis fis )e]<[c8 e a'> f16()e] [d()c b!()a] |
	<[gis8 d' b'> e'16()f]\d[e()d c()b]<\voicetwo a8 e' 
	{\voiceone[d'16()b c()a]}> \onevoice e8 gis |
	['a8. cis16][e g!( f! )e][f( a )d gis,] a8.
	%13
	e,16 <\voicetwo a, cis'' {\voiceone ['e8. f16]}> \onevoice
	\n[g!()e fis()a]\n['cis( d )e bes,][a8.-\trill()g16] |
	[f a''(f)d][g' \n b,()cis a'][g(f e)d][fis d()es c(] |
	%15
	[)bes g'('a )g][fis a d c'][bes('fis)g bes][d()a bes()g] |
	[es(d)es g][c()a bes()g]\d['d(c)d g]\n[bes()'fis g()es] |
	%17
	\n[c(bes)c bes'][a(c)es g,]<['c8-\trill fis'>\d g16()a] ['d8 es16()c] |
	[bes(d )g bes,]['d8 fis]<['g8. g'> a16][bes d g, f] |
	%19
	<\voicetwo bes,8. {\voicetwo[e'-\trill()f16]}> [g()e c()bes]
	\n[a()f' g,()f][e g'( a )bes] |
	% :-(
	% [bes( a g )f^\comma]
	[bes( a g )f^"\\sethuge{\\ '}"]
	[a()'e f()d][bes d(f)a][d'()a bes()g] |
	%21
	['a(\n)g'(\n cis'())d][e()'g a()'es][f()d bes()d]\d['gis f'(e)d] |
	% \n[d( cis b )a^\comma][c(a)fis d'][c a( b )d]
	\n[d( cis b )a^"\\sethuge\\ ,"]
	[c(a)fis d'][c a( b )d]
	[f!(d )'gis d'] |
	%23
	[cis(e g!)bes][e'()a bes()g][f()'cis d()'gis][a8 cis!] |
	['d16 d''(c!)a][bes!(g)e cis'][d a f d] d'8.
}

