\header{
filename =	 "preludes-5.ly";
title =	 "5";
% piece = "\\numfont 5";
piece = "5";
opus = "BWV 926";
% blz 2
% Clavierb"uchlein f"ur W. Fr. Bach
% Clav. W. Fr. Bach: 4-Praeludium 2
% ca 1720
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "public domain";
}
\version "1.0.7";


one = \notes\relative c{
	\property Voice . textstyle =  "italic"
	[d'8-1\p a'-5-"legato" f-3 d-1 a'-5 f-3] |
	[d-1 a'-5 f-3 d-1 a'-5 f-3] |
	[d-1\< bes'-4 g-2 d-1 bes'-4 \!g-2] |
	[d-1 bes'-4 g-2 d-1 bes'-5 g-3] |
	%5
	[cis,-1\mf g'-4 e-2 cis-1 g'-4 e-2] |
	[cis-1 bes'-5 g-4 e-2 a-5 g-4] |
	[f-3\< d-1 f-2 a-4 f-1 a-2] |
	[d-5 a-1 d-2 f-4 d-1 \!f-2] |
	[b\> f-3 e d c-3 b] |
	%10
	[a gis-3 fis e d'-5 \!b-2] |
	< 
		{
			\voiceone 
			\property Voice . textstyle =  "roman"
			c4-\mordent^"(\\textsharp)"\p r r |
			c4-\mordent^"(\\textsharp)" r r
			\property Voice . textstyle =  "italic"
		}
		{ \voicetwo  a4 r r | a4 r r }
	> |
	\onevoice 
	[a'8\mf( es-3\> d c bes!-3 a] |
	[g fis-3 e!-2 d-1 c'-4 \!a] |
	%15
	[)bes8-\mordent\p( d8-5 bes-3 g-1] )g'4 |
	r8 [d-5( c bes a-\prall g] |
	[)a-2 c-4( a f] ) f'4 |
	r8 [c-5 bes a g-\prall f] |
	[g-2 bes-4 a g f-1 e-2] |
	%20
	[f-3 d-1 f-2 a-3 d-5 g,-2] |
	< 
		{ 
			\voiceone 
			% ... textnatural
			[cis8-\prall e8-5-"poco cresc." cis-3 a e' cis]
		}
		{ \voicetwo  a4 }
	>
	\onevoice 
	[a8 e'-5 cis a bes!-3 a] |
	[g e'-5 cis-3 g e'-5 cis-3] |
	[g-1 e'-5 cis g a-3 g] |
	[f-"dim." d'-5 bes f d' bes] |
	%25
	[f d'-5 bes f d' bes] |
	[fis-2-"dim." c'-5 a-3 fis-2 c'-5 a-3] |
	[fis-2 c'-5 a-3 fis-2 c'-5 a-3] |
	[bes-4 g-1 fis-2 g-3 d-1 g-2] |
	[bes-4 g-2 d-1 bes'-5 g-3 d-1] |
	%30
	[e-2\p g-4 fis-3 g-1 bes-4 g-2] |
	[es-1 bes'-4 g-2 es bes' g] |
	[cis,-1 bes'-4 g-2 cis, bes' g] |
	[cis,-1 bes'-4 g-2 cis,-1 a'-5 g-4] |
	[f-3 a f d a'-5 f] |
	%35
	[d-1 a'-5 f-3 d-1 cis-2 d-1] |
	[e-3\< g-5 e-3 bes-1 g'-5 e-3] |
	[bes-1 g'-5 e-3 \!cis-2 a-1 g'-5] |
	[f16-4\mf d-2 c-1 bes-3] \stemup a-2 s16 s8 s4 |
% ugh
%	s1 |
	s4 s4 s4 |
	%40
	s4 s16 [d16-1-"m.d." f-2 a-4] \stemdown [d,-2-"m.g." f a] \stemup d-1 |
	\stemboth
	[f a-4 f d] [f-4 d b d-5] [gis-2 b a gis,] |
	<g'!4.-5\f e a,> a8-5 
	<
		{
			\voiceone 
			f4-4\> ~ | \![f8 e] [e32-4 f e8. ~] [e8 d-3]
		}
		{ \voicetwo  <d4 a> r4 cis-2 }
	> |
	\onevoice 
	[d8-4 c!-3 a-1 d-4 bes-2 g-1] |
	%45
	[c-5 a-3 fis-2 bes-4 g e ] |
	[a-5 fis-"dim. e rall." d g-5 e cis-2] |
	<
		{ \voiceone  <fis2-.-5\p a,> }
		{ \voicetwo  [d32( cis d8. ~] ) d2 }
	> |
	\bar "|.";
}


two = \notes\relative c{
	\clef "bass";
%	[d32( cis )d8.] r4 r |
	d4-\mordent r r |
	d,4 r r |
	d'-\mordent r r |
	d,4 r r |
	d'-\mordent r r |
%	d r r |
	d,4 r r |
	[d'8-2 a-5 d-2 f-1 d-4 f-2] |
	[a-1 f-4 a-2 d-1 a-2 d-1] |
	gis,4-3 r r |
	e-4( )gis-2 e |
	[a8-4 e'-1 c-2 a-4 e'-1 c-2 ] |
	[g!-5 es'-1 c-1 g-5 es'-1 c-1 ] |
	fis,4-5 r r 
	d-4( ) fis d-5 |
	\property Voice . textstyle =  "roman"
	g4-\mordent^"(\\textsharp)" r r8 f!8 |
	\property Voice . textstyle =  "italic"
	e4-\prall r r |
	f4-\mordent r r8 e |
	d4 r8 [f-1 e d] |
	[e-2 d-3 cis-4 e-1 d-2 cis-3] |
	d4-2( )c! bes-4 |
	a( )a' a, |
	a( )a' a, |
	a( )a' a, |
	a( )a' a, |
	bes r r |
	bes-5( )bes' bes, |
	a r r |
	a-2( )d-1 d, |
	g r r |
	g( )g' g, |
	g r r |
	g( )g' g, |
	a r r |
	a( )a' a, |
	a r r |
	a( )a' a, |
	cis, r r |
	cis( )cis' cis, |
	d-4 s16
	\stemdown
	[g'16-1-"m.g." f e] [f-2 a d,-3 f] | 
	a,
	\stemup
	[d-5-"m.d." c bes] a 
	\stemdown [g-2 f-"m.g." e] [d f-4 a-2 d-1] |
	\stemup
%	[f-2\ped a]
	[f-2-"Ped." a]
	\stemdown
%	[d-4-"m.g." f-"cresc."] a s s8 s4\dep |
	[d,-4-"m.g." f-"cresc."] a s s8 s4-"*" |
	\stemboth
% ugh whole rest has duration of one bar
%	r1 |
	r4 r r |
%{
  	ugh forget the 8 below:
	[cis-3( e cis a] [d16-1 c bes a] |
	and lily dumps koor
lilypond: ../flower/include/varray.hh:116: struct Rhythmic_grouping *& Array<Rhythmic_grouping *>::elem(int) const: Assertion `i >=0&&i<size_ failed.
Aborted (core dumped)
%}
	[cis,8-3( e cis a] [d16-1 c bes a] |
	[)g8 g' a-2( g-3 a-1 a,] |
	)d4-3 d'-1 d, |
	d r r |
	d,( )d' d, |
	d2. |
	\bar "|.";
}

global = \notes{
	\time 3/4;
	\key F;
}

treble_staff = \type Staff = treble <
	\global
	\one
>

bass_staff = \type Staff = bass <
% bass = Staff <
	\clef "bass";
	\global
	\two
>

grand_staff = \type GrandStaff <
	\treble_staff
	\bass_staff
>

widea4 = \paper {
	\paper_twenty
%	arithmetic_basicspace = 2.;
%	arithmetic_multiplier = 6.\pt;
	arithmetic_basicspace = 2.;
	arithmetic_multiplier = 8.\pt;
	linewidth= 193.\mm;
}

\score{
        % Moderato
	\grand_staff
	\paper{ \widea4 }
	\midi{ \tempo 4 = 90; }
}

