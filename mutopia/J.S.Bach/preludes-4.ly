\header{
filename =	 "preludes-4.ly";
title =	 "4";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "public domain";
}

rh = \melodic{
	\octave c'';
	\textstyle "italic";
	% ugh, start in multi, because of slurring an tieing over bars
	\multi 2 < 
		{ 
			r16\p_"legato" 
			['d-1( 'fis-3 'd-1] ['a-2 'b-3 cis-4 'a-3]
			[d-3 cis-2 d-1 e-2] [d-1 fis-3 e-2 d-1] |
			\textstyle "finger";
			\stemup; 
			)cis4-2 fis-5 ~ [fis8 fis-4] e4-3 ~ | 
			e( )d8-2 s s4 s4 |
			r8 d4-> cis8-1 ~ cis 'b4-1 'b8 |
		}
		{ 
%			s1
			\stemdown; 
			r8 'a4 'a8 'b4.-"2\\_1" cis8 ~ | 
			cis16\< ['a( cis 'a] [d-2 e fis d-1]
			[g-3 fis g a-4] [a-3 b a \!g] |
			)fis4-"2\\_3" [e8-2 a-5] <fis4-4 d-2> <gis-5 e-3> |
		} 
	>
	\stemboth;
	\bar "|.";
}

lh = \melodic{
	\octave c;
	\clef bass;
	s1 | s1 | s1 | s1
%	\multi 2 <
%		{ \stemup; }
%		{ \stemdown; }
%	>
%	\stemboth; |
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
	}
	\midi{
		\tempo 4 = 110;
	}
}
