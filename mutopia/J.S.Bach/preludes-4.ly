\header{
filename =	 "preludes-4.ly";
title =	 "4";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "public domain";
}

\include "paper20.ly"

one = \melodic{
	\octave c'';
	\textstyle "italic";
	r16\p_"legato" 
	\textstyle "finger";
	['d-1( 'fis-3 'd-1] ['a-2 'b-3 cis-4 'a-3]
	[d-3 cis-2 d-1 e-2] [d-1 fis-3 e-2 d-1] |
	\textstyle "finger";
	\stemup; 
	)cis4-2 fis-5 ~ [fis8 fis-4] e4-3 ~ | 
	e16\< \stemboth ['a16( cis 'a] [d-2 e fis d-1]
	[g-3 fis g a-4] [a-3 b a \!g] |
	\stemup
	)fis4-"2\\_3" [e8-2 a-5] <fis4-4 d-2> <gis-5 e-3> |
	<a4-5( e> <fis-4 d-2> <[)g!16-5 d> fis-2 g-3 a-4] 
	\stemboth
	[g-3 b a g] |
	\textstyle "italic";
	[fis_"dim." e fis-3 g] [fis-3 a-5 g fis]
	\textstyle "finger";
	e4-"2\\_5" ~ e16 ['e( 'fis 'g ] |
	\textstyle "italic";
	['a-4_"dim." 'g-1 'a 'b] ['a-2 c-4 'b 'a]
	['g-1 'fis-2 'g-3 'a] ['g-3 'b 'a 'g] |
	\stemup
	)'fis4\p r16 [''b-1\< d-2 ''b~] <'g4-5 'e-3 ''b>
	r16 ['cis-1 'e-2 \!cis] |  % cis wants ~

}

two = \melodic{
	\textstyle "finger";
% ugh: koor
%	\translator Staff=bass \octave c; \stemup
	fis4-1( e8-2 a4 a4 gis8-2 | 
	) a8
	\translator Staff=treble \octave c''; \stemdown
	'a4 'a8 'b4.-"2\\_1" cis8 ~ | 
	cis8
	\translator Staff=bass \octave c; \stemup
	a8 ~ [a d'] ~ d' d'4-> c'8 | 
	d'8
	\translator Staff=treble \octave c''; \stemdown
	d4-> cis8-1 ~ cis 'b4-1 'b8 |
	r8 'a4 'a8
	\translator Staff=bass \octave c; \stemup
	[g'8-1( fis'-2] )e'4-1 ~ | 
	e'4 d' ~ [d'16 d-1 cis'-2 g-1] cis'4-2 ~ |
	[c'8 a-3] d'4.-1 d'4-> cis'8-2 |
	\translator Staff=treble \octave c''; \stemdown
	'd4 \skip 4*3; |
}

three = \melodic{
	\octave c;
	\stemdown;
	d4-3 c-4 'b e-3 |
	a16 ['a-5\mf( c-3 'a-5] [d-2 e-1 fis-2 d-4]
	[g-1 fis-3 g a] [g b a g] |
	)fis4 fis e a-4 |
	d'16 \stemboth [d-5\f( fis-3 d-5] [a-2 b-1 c'-2 a-4]
	[d'-1 c'-3 d' e'] [d'-4 fis'-2 e'-1 ) d'] |
	[cis'-3 b-4 cis'-3 d'-2] [c'-4 e'-2 d'-3 c'-4] 
	\stemdown b4-5 [cis'8-4 b-3(] |
	[a-4 )fis-5 b-2 a-3] [gis-4 e-5 a-3 g-4] |
	fis4. d8-5 e4-5 a4-3 |
	\stemboth
	r16 [d-4( fis-2 d-4] [)g8-.-1 'g-.-5]
	r16 [e-4( g-2 e-4] [)a8-. 'a-.-5] |
}

rh = \melodic{
	\one
	\bar "|.";
}


lh = \melodic{
	\clef "bass";
	\multi 2 < 
		\two
		\three
	>
	\bar "|.";
}


global  = \melodic{
	\meter 4/4;
	\key fis cis;
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
%			\two
%			\three
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
