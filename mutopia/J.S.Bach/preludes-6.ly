\header{
filename =	 "preludes-6.ly";
title =	 "6";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "public domain";
}

\include "paper20.ly"

one = \melodic{
	\octave c';
	\skip 4*4; |
	\stemup
	r4 d2-5( cis4-4 |
	\stemboth
	[)d16-5\mf a'-4( g' f'] [e' f'-4 cis'-2 d'-1] 
	[e32-3 d e8 f16] [d'8.-1 cis'16-3] |
	\stemup
	)cis'4\> ~ [cis16 a \!d'8 ~] d4 c-5 |
	b2-4 ~ b4 a-5 ~ |
	[a16 a-5 \stemup g! f] g4-4\< ~ g f-3 ~ |
	[\!f16 a g f] [e16 g8.-5 ~] [g16 g-5 f-4 e-3] [d-1 f8.-4 ~] |
	[f16 f-3 e d] b4 a-5 g-5 |
	fis4-4 g r8\<-"rall." [g16-1( bes-2] r16 [bes-2 a-2 \!g-1] |
	) d'1-5
}

two = \melodic{
	\octave c';
	r16\p [d'-5( c'-4 bes-3] [a-2 bes-3 f-1 g-2] 
	[a32-3 g a8 bes16-1] [g8. f16-1] |
	)f2 e2 |
	\translator Staff=bass \octave c; \stemup
	r4 a-1 bes-2 b-1 |
	\translator Staff=treble \octave c'; \stemdown
	r16 [b-3 a-1 g-2] [f8. f16-2] e2 ~ |
	e2 ~ e4 ~ [e16 e-2 f! d-1] |
	s4 [e32 d e8.~] e4 d4 ~ |
	d4. [cis16-2 d-1] cis4 d-1 ~ |
	d8 r r16 [e-2 f d] r16 [e-2 f d] r [d-1 e-3 cis] |
	r16 [e-3 d-1 c!-2] ['bes! d8.] r8 e'4-5 |
	fis1-2
}

three = \melodic{
	\octave c;
	\stemup
	f2-1( e-2 |
	[)d16 b'( c' bes] [a-4 bes-2 f-5 g-4] 
	[a32 g a8 bes16-2]
	[g8.-4 f16-5] |
	)f2-5 g4-4 gis |
	a2-2 ~ [a16 a-1( g f] [e-4 f-2 c-5 d] |
	[e32 d e8 f16] [d8.-4 c16-5] \stemboth )c4.-5 d8-4 |
	e4 ~ [e16 f-2( e-3 d-4] [cis 'a 'b cis-3] [d-4 e-3 f-2 d-4] |
	\textstyle "finger";
	)'bes!2-"5\\_4" 'a ~ |
	a a |
	'd cis-5 |
	b1-2
}

four = \melodic{
	\octave c;
	d2-3 cis-4 |
	\skip 4*16;
	\stemup
	b2-1 a-1 |
	g a4. [gis16 a] |
	gis2 <[g8 cis> <f-3 d-1]> e4-2 |
	d4. [fis16-3 g-2] r16 b8.-1 ~ b4 |
	\stemdown
	d1-5
}

rh = \melodic{
	\one
	\multi 2 < 
		\one
		\two
	>
	\bar "|.";
}


lh = \melodic{
	\clef "bass";
	\multi 2 <
	      \three
	      \four
	>
	\bar "|.";
}


global  = \melodic{
	\meter 4/4;
	\key bes;
}

\score{
	% Allegretto
	% it would be nice to shut-off fingering...
	\melodic \type Grandstaff < 
		\type Staff=treble  < 
			\global 
% huh? try these iso directly!
			\lh
%			\one
%			\two
		>
		\type Staff=bass  < 
			\global 
			\rh
% or try \two having here, iso above!
%			\three
%			\four
		>
	>
	\paper{
		\paper_twenty
		linewidth= 195.\mm;
	}
	\midi{
		\tempo 4 = 110;
	}
}
