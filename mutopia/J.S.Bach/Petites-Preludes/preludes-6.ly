\header{
filename =	 "preludes-6.ly";
title =	 "6";
% piece = "\\numfont 6";
piece = "6";
opus = "BWV 940";
% blz 11
% Six Petits Preludes
% Collection Johann Peter Kellner
% ca 1703 - 1707
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "public domain";
}
\version "1.0.7";


one = \notes\relative c{
	\skip 1; |
	\stemup
	r4 d''2-5( cis4-4 |
	\stemboth
	[)d16-5\mf a'-4( g f] [e f-4 cis-2 d-1] 
	[e8.-\mordent f16] [d8.-\prall-1 cis16-3] |
	\stemup
	)cis4\> ~ [cis16 a \!d8 ~] d4 c-5 |
	%5
	b2-4 ~ b4 a-5 ~ |
	[a16 a-5 \stemup g! f] g4-4\< ~ g f-3 ~ |
	[\!f16 a g f] [e16 g8.-5 ~] [g16 g-5 f-4 e-3] [d-1 f8.-4 ~] |
	[f16 f-3 e d] b'4 a-5 g-5 |
	fis4-4 g r8\<-"rall." [g16-1( bes-2] \!e4-5 |
	%10
	\!)d1-5
	\bar "|.";
}

two = \notes\relative c{
	r16\p [d''-5( c-4 bes-3] [a-2 bes-3 f-1 g-2] 
	[a8.-\mordent bes16-1] [g8.-\prall f16-1] |
	\stemdown
	)f2 e2 |
	\translator Staff=bass \stemup
	r4 a,-1 bes-2 b-1 |
	\translator Staff=treble \stemdown
	r16 [b'-3 a-1 g-2] [f8. f16-2] e2 ~ |
	%5
	e2 ~ e4 ~ [e16 e-2 f! d-1] |
	s4 [e32 d e8.~] e4 d4 ~ |
	d4. [cis16-2 d-1] cis4 d-1 ~ |
	d8 r r16 [e-2 f d] r16 [e-2 f d] r [d-1 e-3 cis] |
	r16 [e-3 d-1 c!-2] [bes! d8.] s4 r16\> [bes'-2 a-2 \!g-1] |
	%10
	fis1-2
}

three = \notes\relative c{
	\stemup
	f2-1( e-2 |
	\stemboth
	[)d16 d'( c bes] [a-4 bes-2 f-5 g-4] 
	[a8.-\mordent bes16-2]
	[g8.-\prall-4 f16-5] |
	)f2-5 g4-4 gis |
	a2-2 ~ [a16 a-1( g f] [e-4 f-2 c-5 d] |
	%5
	[e8.-\mordent f16] [d8.-\prall-4 c16-5] \stemboth )c4.-5 d8-4 |
	\stemdown
	e4 ~ [e16 f-2( e-3 d-4] [cis a b cis-3] [d-4 e-3 f-2 d-4] |
	\property Voice . textstyle =  "finger"
	)bes!2-"5\\_4" a ~ |
	a a |
	d, cis'-5 |
	%10
	a'1-2
	\bar "|.";
}

four = \notes\relative c{
	\stemdown 
	d2-3 cis-4 |
	\skip 1*3; |
	%5
	\translator Staff=treble \stemup \property Voice.hshift = 1 
	a'4 gis-2 ~ [gis16 gis-3 fis e]
	\skip 4*1;
	\translator Staff=bass \stemdown \property Voice.hshift = 0 
	\stemup
	b2-1 a-1 |
	g a4. [gis16 a] |
	gis2 <[g8 cis,> <f-3 d-1]> e4-2 |
	d4. [fis16-3 g-2] r16 bes8.-1 ~ bes4 |
	%10
	\stemdown
	d,1-5
}

global = \notes{
	\time 4/4;
	\key F;
}


treble_staff = \type Staff = treble <
	\global
	\one
	\two
>

bass_staff = \type Staff = bass <
% bass = Staff <
	\clef "bass";
	\global
	\three
	\four
>

grand_staff = \type GrandStaff <
	\treble_staff
	\bass_staff
>

a4 = \paper{
	linewidth = 193.\mm;
}

\score{
        % Allegretto
	\grand_staff
	\paper{ \a4 }
	\midi{ \tempo 4 = 40; }
}

