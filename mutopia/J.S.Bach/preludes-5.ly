\header{
filename =	 "preludes-5.ly";
title =	 "5";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "public domain";
}

\include "paper20.ly"

rh = \melodic{
	\octave c';
	\textstyle "italic";
	[d8-1\p a-5-"legato" f-3 d-1 a-5 f-3] |
	[d-1 a-5 f-3 d-1 a-5 f-3] |
	[d-1\< b-4 g-2 d-1 b-4 \!g-2] |
	[d-1 b-4 g-2 d-1 b-5 g-3] |
	[cis-1\mf g-4 e-2 cis-1 g-4 e-2] |
	[cis-1 b-5 g-4 e-2 a-5 g-4] |
	[f-3\< d-1 f-2 a-4 f-1 a-2] |
	[d'-5 a-1 d'-2 f'-4 d'-1 \!f'-2] |
	[b'\> f'-3 e' d' c'-3 b] |
	[a gis-3 fis e d'-5 \!b-2] |
	\multi 2 < 
		{
			\stemup
%			c4\mr^"(\\textsharp)"\p r r |
%			c4\mr^"(\\textsharp)" r r
			c4^"(\\textsharp)"\p r r |
			c4^"(\\textsharp)" r r
		}
		{
			\stemdown
			a4 r r |
			a4 r r
		}
	> |
	[a'8\mf( es'-3\> d' c' bes!-3 a] |
	[g fis-3 e!-2 d-1 c'-4 \!a] |
	[)bes32(\p a bes16] [d'8-5 bes-3 g-1] )g'4 |
%	r8 [d'-5( c bes a\pr g] |
	r8 [d'-5( c bes a g] |
	[)a-2 c'-4( a f] ) f'4 |
%	r8 [c'-5 bes a g\tr f] |
	r8 [c'-5 bes a g f] |
	[g-2 bes-4 a g fis-1 e-2] |
	[f-3 d-1 f-2 a-3 d'-5 g-2] |
	\multi 2 < 
		{
			\stemup
			[cis'32-4 b cis'16 e'8-5-"poco cresc." cis-3 a e' cis]
		}
		{
			\stemdown
			a4
		}
	>
	\stemboth
	[a8 e'-5 cis' a bes!-3 a] |
	[g e'-5 cis'-3 g e'-5 cis'-3] |
	[g-1 e'-5 cis' g a-3 g] |
	[f-"dim." d'-5 bes f d' bes] |
	[f d'-5 bes f d' bes] |
	[fis-2-"dim." c'-5 a-3 fis-2 c'-5 a-3] |
	[fis-2 c'-5 a-3 fis-2 c-5 a-3] |
	[bes-4 g-1 fis-2 g-3 d-1 g-2] |
	[bes-4 g-2 d-1 bes-5 g-3 d-1] |
	[e-2\p g-4 fis-3 g-1 bes-4 g-2] |
	[es-1 bes-4 g-2 es bes g] |
	[cis-1 bes-4 g-2 cis bes g] |
	[cis-1 bes-4 g-2 cis-1 a-5 g-4] |
	[f-3 a f d a-5 f] |
	[d-1 a-5 f-3 d-1 cis-2 d-1] |
	[e-3\< g-5 e-3 'bes-1 g-5 e-3] |
	['bes-1 g-5 e-3 \!cis-2 'a-1 g-5] |
	[f16-4\mf d-2 c-1 'bes-3] s4 s |
% ugh
%	s1 |
	s4 s4 s4 |
	s4 s16 [d16-1-"m.d." f-2 a-4] [d-2-"m.g." f a] \stemup d'-1 |
	[f' a'-4 f' d'] [f'-4 d' b d'-5] [gis-2 b a g] |
	<g'!4.-5\f e' a> a'8-5 
	\multi 2 <
		{
			\stemup
			f'4-4\> ~ | \![f'8 e'] [e'32-4 f' e'8. ~] [e'8 d'-3]
		}
		{	
			\stemdown
			<d'4 a> r4 cis'-2
		}
	> |
	\stemboth;
	[d'8-4 c'!-3 a-1 d'-4 bes-2 g-1] |
	[c'-5 a-3 fis-2 bes-4 g e ] |
	[a-5 fis-"dim. e rall." d g-5 e cis-2] |
	\multi 2 <
		{
			\stemup
			<fis2-.-5\p 'a>
		}
		{	
			\stemdown
			[d32( cis d8. ~] ) d2
		}
	> |
	\bar "|.";
}


lh = \melodic{
	\clef "bass";
	\octave c;
	[d32( cis )d8.] r4 r |
	'd4 r r |
%	d\mr r r |
	d r r |
	'd4 r r |
%	d\mr r r |
	d r r |
	'd4 r r |
	[d8-2 'a-5 d-2 f-1 d-4 f-2] |
	[a-1 f-4 a-2 d'-1 a-2 d'-1] |
	gis4-3 r r |
	e-4( )gis-2 e |
	[a8-4 e'-1 c'-2 a-4 e'-1 c'-2 ] |
	fis4-5 r r 
	d-4( ) fis d-5 |
%	g\mr-"(\textsharp)" r r f!8 |
	g-"(\textsharp)" r r8 f! |
	[e32 f e8.] r4 r |
%	f\pr r r8 e |
	f r r8 e |
	d4 r8 [f-1 e d] |
	[e-2 d-3 cis-4 e-1 d-2 cis-3] |
	d4-2( )c! 'b-4 |
	'a( )a a |
	'a( )a a |
	'a( )a a |
	'a( )a a |
	'b r r |
	'b-5( )b 'b |
	'a r r |
	'a-2( )d-1 'd |
	'g r r |
	'g( )g g |
	'g r r |
	'g( )g g |
	'a r r |
	'a( )a a |
	'a r r |
	'a( )a a |
	'cis r r |
	'cis( )cis 'cis |
	'd-4 s16
	\stemdown
	[g16-1-"m.g." f e] [f-2 a d-3 f] | 
	'a
	\stemup
	[d-5-"m.d." c 'bes] 'a 
	\stemdown ['g-2 'f-"m.g." 'e] ['d 'f-4 'a-2 d-1] |
	\stemup
%	[f-2\ped a]
	[f-2-"Ped." a]
	\stemdown
%	[d-4-"m.g." f-"cresc."] a s s8 s4\dep |
	[d-4-"m.g." f-"cresc."] a s s8 s4-"*" |
% ugh, whole rest has duration of one bar
%	r1 |
	r4 r r |
%{
  	ugh, 'forget' the 8 below:
	[cis-3( e cis 'a] [d16-1 c 'b 'a] |
	and lily dumps koor
lilypond: ../flower/include/varray.hh:116: struct Rhythmic_grouping *& Array<Rhythmic_grouping *>::elem(int) const: Assertion `i >=0&&i<size_' failed.
Aborted (core dumped)
%}
	[cis8-3( e cis 'a] [d16-1 c 'b 'a] |
	[)'g8 g a-2( g-3 a-1 'a] |
	)d4-3 d'-1 d |
	d r r |
	'd( )d 'd |
	'd2. |
	\bar "|.";
}

global  = \melodic{
	\meter 3/4;
	\key bes;
}

\score{
	% Moderato
	% it would be nice to shut-off fingering...
	\melodic \type Grandstaff < 
		<
			\global 
			\rh
		>
		<
			\global 
			\lh
		>
	>
	\paper{
		\paper_twenty
		linewidth= 195.\mm;
	}
	\midi{
		\tempo 4 = 90;
	}
}
