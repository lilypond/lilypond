% remark: still temporarily broken. 

\header{
filename =	 "coriolan-alto.ly";
title =	 "Ouverture zu Collins Trauerspiel "Coriolan" Opus 62";
description =	 "a 5 bar fragment of the alto part";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";

TestedFeatures:
}

%
% when lily can handle this, i-ll type the other 304 bars 
% (maybe even sooner :-)
%

\version "0.1.8";

alto1 = \melodic{
	\meter 4/4; 
	\key bes es as;
		
	\clef "alto";
	% these are chords (i.e. non-divisi)
	{ \octave  c ;
			
	[ c8 c8-. ] c2.( | 
%31
	[ ) c8 c8-. ] c2.( | 
%32
	[ ) c8 c8-. ] c4. c8-. c4( |
%33
	[ ) c8 c8-. ] c4. c8-. c4  |

	%% divisi
	\type Voice="altodivisi1" {\stemup c4:16 ^"div." c4: c2: }
	\stemboth
	}
%30:2
}

alto2 = \melodic{ 
		\octave  'c ;
		\textstyle "italic";
		% this should be a \dynamic, rather than text
		[ c8 c8_"cresc. \ \ - \ \ \ \ - \ \ \ \ - \ \ \ \ 
			- \ \ \ \ - \ \ \ \ - \ \ \ \ - \ \ \ \ 
			- \ \ \ \ - \ \ \ \ - \ \ \ \ - \ \ \ \ 
			- \ \ \ \ - \ \ \ \ -" ] c2.( |  
		\textstyle "roman";
%31:2
		[ ) c8 c-. ] c2.( | 
%32:2
		[ ) c8 c-. ] c4. c8-. c4( | 
%33:2
		[ ) c8 c-. ] c4. c8-. c4 |
		\octave c ;

%34		
		% these are two \voices (i.e. divisi)
		% the c4 and c2, however are really 16th...
			% we-ll say "simile" for now...
	% 
		\octave  'c ;
		\type Voice="altodivisi2" {
		\stemdown
		as4: as4: as2}
		\stemboth
}

\score{
	\type Voice < \alto1 \alto2 >
	\paper{ 
	}
}
