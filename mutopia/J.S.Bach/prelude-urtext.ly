% prelude-urtext.ly
% belongs together with -cello.ly and -alto.ly
% (who is going to make a -violin.ly?)

%{
Well, there are still some scripts in this "urtext".
But merging music and scripts doen't work too well yet (see alto_scripts).
%}

n = { \slurnormal }
d = { \slurdotted }

%
% this must be redone a bit:
% 
%  * two voices: see allemande/sarabande
%  * beams in different beam-thread: see first few bars
%  * slurs/ties in two different threads: urtext, and additionals
%  

%prelude_notes = \melodic{
prelude_notes = \type Thread = prelude \melodic{
	d,8 f a4 ~ a16 f( e )d |
	cis( e g )a bes4 ~ bes16 a( g )f |

	[e( g a )cis] \d [e8.() bes,16] \n [a16( g f )e] |
	[f( g a )f] [d8( c! bes )a] |
	%5
	[bes16 d( f )a] \d [d8.()c16] [bes( a g )f] |
	[e( g bes )d] [c( a bes )g] [f( e )g bes,] |
	\n [a c( e )g] \d [c8.()bes16] \n [a( g f )e] |
	\d [d( f a )c] [bes( g a )f] \n [e( f a )f] |
	%9
	\d [g, bes( d )f] [bes8.() a16] \n [g( f e )g] |
	[a, c( e )g] [c a( e )g] [f( a )d, es] |
	[d a( bes )d] [g bes( a )c] \d [bes( g )d' f,] |
	%12
	\n[e b( c )e] \d[g d( e )c] [bes( g )e' bes,] |
	[f8 a] \n c4 ~ [c16 bes( a )g] |
	[fis a( bes )c] [d, c'( bes )a] [fis' es( d )c] |
	\d [bes( a )g bes] \n d4 ~ [d16 \d c( bes )a] |
	%16
	\n[gis b( c )d] [e, d'( c )b] [gis' \d f ( e )d] |
	[c( b a )c] [f( e f )gis] [a( f d )c] |
	\n [b( d gis' )b] \d[d8.()c16] \n[b( a gis )a] |
	%19
	\d [c,( e a )c] [e8.()c16] \n [b( a gis )a] |
	[d,( f a )d] \d[f8.()e16] \n[d( c b )d] |
	\d[e, d'( c )b] \n[a c( b )a] [d, b'( a )gis] |
	%22
	[c, a'( g!)f] [cis g,( f )e] [d f( e )d] |
	[gis' \d d'( e )f] [b' f,( e )d] [gis,\n d'( c )b] |
	\d[a( b c )e] [a( b c )a] [e( c a )g!] |
	% 25
	% B"arenreiter and Chester say "c bes c"
	[fis \n a( c )d] \d[es8.()d16] \n[c( bes )c a'] |
	[bes,( a )bes d] [g, es'( f )g] [a, g'( f )es] |
	[d( c )d f] [bes,  g'( a )bes] [cis, bes'( a )g] |
	% 28
	[f( e )f a] [d, bes'( c! )d] [e,! \d d'( c )bes] |
	\n[a( g )a c] [f, d'( e )f] [g, f'( e )d ] |
	[cis g,( f )e] [a, e'( f )g] [cis'( bes! )a g] |
	%31
	[f( g )a cis] [d a( g )f] [a f( e )d] |
	\d[gis' d,( e )f] [ a, f'( e )d] [gis'( f! )e d ] |
	[cis( b )cis e] [a \n e( cis )e] [a, g'!( f )e] |
	%34
	[f( e )f a] [d a( f )a] \n[d, c'!( bes )a] |
	[g( f )g cis'] [e \d cis( g, )cis'] [a,, g'( f )e ] |
	[d a' d e] [f d a f] [d c'!( bes )a] |
	%37
	\n[g( a )bes d,] [es f g a] [bes g es' g,] |
	\d[f( g )a cis,][ d e! f g][a f d' f,] |
	[e( f )g bes,][a b cis d] \d[e bes, g' bes,] |
	%40
	[cis,8 a'] \n g'4 ~ [g16 bes( a )g] |
	[f( e d )e] [f d a' f] [d' a f d] |
	[gis,8 f'] d'4 [d16 f( e )d] | 
	[cis( b )a b][cis a d a][e' a, f' a,] |
	%44
	[g' \d e( cis )e][a,( cis )e f][g f g e] |
	[f d( cis )d][a( cis )d e][f e f d] |
	[e cis( b )cis]\n[a( b )cis d][e d e cis] |
	%47
	[d \n b( a )b][f,(gis )b cis][d cis d b] |
	<{\voiceone <d,4^\fermata cis'>}{\voicetwo g,,}>\onevoice r r |
	[bes''16 g( fis)g][es g d g]\d[es( g)bes d,] |
	[cis--( \n e!( g ))a]\d[bes8.()a16]\n[g( fis )g e'] |
	%51
	[f,! d' bes g]\d[a()f e()g][f()d cis()e] |
	[d bes( a )g][fis--( \n a( c! ))es]\d[d( c bes )a] |
	[bes g( fis )g][es g d g]\n[es( g )bes d,] |
	%54
	<\voicetwo cis4 bes' {\voiceone [g'8.( )f16] }> 
	\n[e!( d cis b][a g f )e] \onevoice |
	\d[d--( \n a'( d ))e][f( e d c!][bes! a g )f] |
	\d[e--( \n a( cis ))e][g( f e d][cis b a )g] |
	[f a( d )f][a d,( f )a][d bes! c! a] |
	[g,, \d d'( g )a][bes \n g( fis)g][es' g, d' g,] |
	<{\voiceone <g2. cis'> | <f,2. d'> | <e, d'> | <e, cis'> | <f, d' > | }
	{\voicetwo a,,2.  a a a <d, a'>}>
	\octave c';
}

prelude_beam1 = \melodic { [s8 s]s4[s16 s s s] }
prelude_beam2 = \melodic { [s16 s s s]s4[s16 s s s] }

prelude_beams = \type Thread = prelude \melodic{
	\$prelude_beam1
	\$prelude_beam2
}

prelude = \type Voice \melodic<
	\$prelude_notes
	\$prelude_beams
>

