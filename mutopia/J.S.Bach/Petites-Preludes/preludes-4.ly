\header{
filename =	 "preludes-4.ly";
title =	 "4";
opus = "BWV 925";
% piece = "\\numfont 4";
piece = "4";
% blz 8
% Clavierb"uchlein f"ur W. Fr. Bach
% Clav. W. Fr. Bach: 27-Praeludium ex d neutral
% ca 1720
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "public domain";
}
\version "1.0.7";

one = \notes\relative c{
	\property Voice . textstyle =  "italic"
	r16\p_"legato" 
	\property Voice . textstyle =  "finger"
	[d'-1( fis-3 d-1] [a'-2 b-3 cis-4 a-3]
	[d-3 cis-2 d-1 e-2] [d-1 fis-3 e-2 d-1] |
	\property Voice . textstyle =  "finger"
	\stemup  
	)cis4-2 fis-5 ~ [fis8 fis-4] e4-3 ~ | 
	e16\< \stemboth [a,16( cis a] [d-2 e fis d-1]
	[g-3 fis g a-4] [g-3 b a \!g] |
	\stemup
	)fis4-"2\\_3" [e8-2 a-5] <fis4-4 d-2> <gis-5 e-3> |
	%5
	<a4-5( e> <fis-4 d-2> <[)g!16-5 d> fis-2 g-3 a-4] 
	\stemboth
	[g-3 b a g] |
	\property Voice . textstyle =  "italic"
	[fis_"dim." e fis-3 g] [fis-3 a-5 g fis]
	\property Voice . textstyle =  "finger"
	e4-"2\\_5" ~ e16 [e,( fis g ] |
	\property Voice . textstyle =  "italic"
	[a-4_"dim." g-1 a b] [a-2 c-4 b a]
	[g-1 fis-2 g-3 a] [g-3 b a g] |
	\stemup
	)fis4\p 
	\skip 4*7; |
	%10
	\property Voice . textstyle =  "finger"
	d'4.\mf cis8-"4\\_5" ~ cis b4-5 a8-"4\\_5" ~ |
	\property Voice . textstyle =  "italic"
	a g4-5 fis8-4 ~ [fis16 fis-4-"dim." e-3 d-2] 
	\property Voice . textstyle =  "finger"
	e4-"3\\_5" ~ |
	[e16 e-5 d cis] d4-5 ~ [d16 d-5 cis b] cis4-5 |
	\stemboth
	[fis,16-1\p a-2 d-5\< c-4] [b-1 d-2 g-5 fis-4]
	[d-1 b'-5 a g] [fis-2 e-1 d-4 \!c!-3] |
	[b-2 d-1 g a] [fis8-\prall e16-4 d-3]
	[d8. e16~] [e d8 cis16] |
	%15
	\stemup
	r16 [d-1\< fis d] [g a b g-1] [c-3 b c d] [c e d-4 \!c] |
	b4-5 a ~ [a8 g-5 ~] [g16 fis8-4 e16-3] |
	[fis8-4 e-5~] [e d-5~] d4 cis-4 |
	d2\p-"rall." ~ [d16 a-2( b-3 cis-4] )d4-5 |
	\bar "|.";
}

two = \notes\relative c{
	\property Voice . textstyle =  "finger"
	fis4-1( e8-2 a4 a4 gis8-2 | 
	) a8
	\translator Staff=treble \stemdown
	a'4 a8 b4.-"2\\_1" cis8 ~ | 
	cis8
	\translator Staff=bass \stemup
	a,8 ~ [a d] ~ d d4-> cis8 | 
	d8
	\translator Staff=treble \stemdown
	d'4-> cis8-1 ~ cis b4-1 b8 |
	%5
	r8 a4 a8
	\translator Staff=bass \stemup
	[g8-1( fis-2] )e4-1 ~ | 
	e4 d ~ [d16 d-1 cis-2 b-1] cis4-2 ~ |
	[cis8 a-3] d4.-1 d4^> cis8-2 |
	\translator Staff=treble \stemdown
	d4 \stemboth r16 [b-1\< d-2 b~] <g'4-5 e-3 b>
	r16 [cis,-1 e-2 \!cis~] |
	<a'4-5 fis-3 cis> r16 [d,-1 fis-2 d~] <b'4-5 g-3 d>
	r16 [fis-1 a fis~] |
	%10
	\stemdown
	fis4 e d cis |
	b a b4. b8 |
	a4
	\translator Staff=bass \stemup
	r16 [b-3 fis-1 a-2] g4-1 r16 [a-3 e-1 g-2] |
	\skip 1*1; 
	s16
	\translator Staff=treble \stemdown
	d'8.-1 ~ [d8 c] d4 [a8 g] |
	%15
	\translator Staff=bass \stemup
	[fis'8 c-1] b4-1 \stemdown <a
	\translator Staff=treble \stemdown
	a'4.-2>
	\stemdown
	a'8~ |
	[a g-3~] [g16 e fis8~] [fis16 d8.~] [d8. cis!16] |
	\translator Staff=bass \stemup
	<
	%	{ \voiceone  [d,8-2 a~] a4 }
		{ \voiceone  [d8-2 a~] a4 }
	%	{ \voicethree  [a,8-1 g] [fis16-2 e-1 fis-2 d-1] }
		{ \voicethree  [a8-1 g] [fis16-2 e-1 fis-2 d-1] }
	>
	\voiceone 
	[g-1 fis-2 g-1 a-2] [g-1 b-3 a-2 g-1 ~] |
	[g g-1 fis-2 e-1] fis4-2 ~ fis2
	\bar "|.";
}

three = \notes\relative c{
%	\stemdown 
	d4-3 c-4 b e-3 |
	a16 [a,-5\mf( cis-3 a-5] [d-2 e-1 fis-2 d-4]
	[g-1 fis-3 g a] [g b a g] |
	)fis4 fis e a-4 |
	d16 \stemboth [d,-5\f( fis-3 d-5] [a'-2 b-1 cis-2 a-4]
	[d-1 cis-3 d e] [d-4 fis-2 e-1 ) d] |
	%5
	[cis-3 b-4 cis-3 d-2] [c-4 e-2 d-3 c-4] 
	\stemdown b4-5 [cis8-4 b-3(] |
	[a-4 )fis-5 b-2 a-3] [gis-4 e-5 a-3 g-4] |
	fis4. d8-5 e4-5 a4-3 |
	\stemboth
	r16 [d,-4( fis-2 d-4] [)g8-.-1 g,-.-5]
	r16 [e'-4( g-2 e-4] [)a8-. a,-.-5] |
	r16 [fis'-4( a fis] [)b8-. b,-.] r16 [g'-4( b g] [)d'8-. d,-.] |
	%10
	r16 [d,16-5( fis-3 d] [a'-2 b-1 cis-2 a-4] [d b-5 d-3 b]
	[fis'-2 g a-2 fis-4] |
	)fis16 [g,( b g] [d'-2 e fis-2 d-4] [)g8-. g,-.] r16 [cis-4( e cis] |
	[)fis8-. fis,-.] \stemdown r16 [b-4( d b] [)e8-. e,-.] 
	r16 [a-4 cis a] |
	\stemboth
	[d-1 e-3 fis-2 d-4] [g-1 fis-2 e-3 d-1] [cis!-3 a-5 b cis]
	[d-2 e fis-2 d-4] |
	[g8 e-3] \stemdown [a a,] [b8.-4 g16-5] a4-4 |
	%15
	[d8-5 a'-2~] [a g-3~] [g g-3] fis4-4 |
	\stemup
	r16 [g-2 b-1 g-3] [d'8.-1 c16-1] [b8.-2 bes16-3] [a8-1 g] |
	\stemdown
	r16 [a,-2 cis!-1 a'-2] [d,-1 e fis d] [e8-1 d-2] [e-1 a,-2] |
	[d a-2~] [a16 a-2 b cis-2] d2 |
}

four = \notes\relative c{
	\skip 1*9;
%	\stemup 
%	\property Voice.hshift = 1 
	%10
	a''2 fis |
	d s |
%	\property Voice.hshift = 0 
	\skip 1*2; |
	s4
	\translator Staff=bass \stemup
	a4 ~ [a16 d,-2 g8-1] [fis e-1] |
	%15
	\stemdown
	d1 ~ | 
	d ~ | 
	d, ~
	d2 d2
}

global  = \notes{
	\time 4/4;
	\key D;
}

treble_staff = \type Staff = treble <
	\global
	{ \voiceone \one }
	{ \voicefour \four }
>

bass_staff = \type Staff = bass <
% bass = Staff <
	\clef "bass";
	\global
%	{ \voiceone \two }
	{ \type Voice = bone \skip 1*0; \property Voice.ydirection = \up \two }
	{ \voicetwo \three }
>

grand_staff = \type GrandStaff <
	\treble_staff
	\bass_staff
>

\score{
        % Allegretto
	\grand_staff	
	\include "preludes-paper.ly";
	\midi{ \tempo 4 = 70; }
}
