% Ludwig van Beethoven (1770-1792)
%
% Opus 62
% Ouverture zu Collins Trauerspiel "Coriolan"
%
% a 5 bar fragment of the alto part
%
% Copyright (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
%
% when lily can handle this, i-ll type the other 304 bars 
% (maybe even sooner :-)
%
\version "0.0.54";

alto = \melodic{ 
	
	\meter 4/4; 
	\duration  8 ;
	\key bes es as;
		
	\clef "alto";
	% these are chords (i.e. non-divisi)
	
%30     
	< 	
		{ \octave  c ;
			
			[ c c-. ] c2.( | 
%31
			[ ) c c-. ] c2.( | 
%32
			[ ) c c-. ] c4. c-. c4( |
%33
			[ ) c c-. ] c4. c-. c4 
		\group "+bla";
		 \stem  1 ;[ c16^"div." c16 c16 c16 ] c4 c2 
		\group "-";
	  	}
%30:2
	  	{ 
			\octave  'c ;
			\textstyle "italic";
			% this should be a \dynamic, rather than text
	  		[ c c_"cresc. \ \ - \ \ \ \ - \ \ \ \ - \ \ \ \
				- \ \ \ \ - \ \ \ \ - \ \ \ \ - \ \ \ \
				- \ \ \ \ - \ \ \ \ - \ \ \ \ - \ \ \ \
				- \ \ \ \ - \ \ \ \ -" ] c2.( |  
			\textstyle "roman";
%31:2
			[ ) c c-. ] c2.( | 
%32:2
			[ ) c c-. ] c4. c-. c4( | 
%33:2
			[ ) c c-. ] c4. c-. c4 |
			\octave c ;

%34		
			\group "+b";
	% these are two \voices (i.e. divisi)
	% the c4 and c2, however are really 16th...
	% we-ll say "simile" for now...
	% 
			\octave  'c ;
			\textstyle "italic";
			\stem  -1 ;
			[ as16 \ff as16 as16 as16 ] as4_"simile" as2
			\textstyle "roman";
			\group "-";
		}
	>
	
}

\score{
	\staff{ melodicregs alto }
	\paper{ 
		\unitspace 14\mm
		\geometric 1.4
	}
}
