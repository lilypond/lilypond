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

alto = music { 
	$
	\duration{ 8 }
	\key {bes es as}
		
	\clef "alto"
	% these are chords (i.e. non-divisi)
	% that-s why i-d rather type chords, 
	% but how to type slurs?
%30     
		{ 	
		\music{ 
			[ c c-. ] c2.( | 
%31
			[ ) c c-. ] c2.( | 
%32
			[ ) c c-. ] c4. c-. c4( |
%33
			[ ) c c-. ] c4. c-. c4 
		}
%30:2
	  	\music{ 
			\octave{ ` }
			\textstyle "italic"
			% this should be a dynamic, rather than text
	  		[ c c_"cresc. \ \ - \ \ \ \ - \ \ \ \ - \ \ \ \
				- \ \ \ \ - \ \ \ \ - \ \ \ \ - \ \ \ \
				- \ \ \ \ - \ \ \ \ - \ \ \ \ - \ \ \ \
				- \ \ \ \ - \ \ \ \ -" ] c2.( |  
			\textstyle "roman"
%31:2
			[ ) c c-. ] c2.( | 
%32:2
			[ ) c c-. ] c4. c-. c4( | 
%33:2
			[ ) c c-. ] c4. c-. c4 |
			\octave{ }
	  	}
	}
%34
	% these are two voices (i.e. divisi)
	% the c4 and c2, however are really 16th...
	% e.g.: c16*4 and c16*8  or
	%       [ c4 c2 ]*16 ?
	% we-ll say "simile" for now...
	% 
		{ \multivoice
		\music{ \stem{ 1 }[ c16^"div." c16 c16 c16 ] c4 c2 }
	  	\music  { 
			\octave{ ` }
			\textstyle "italic"
			\stem{ -1 } [ as16_"\ff" as16 as16 as16 ] as4_"simile" as2
			\textstyle "roman"
		}
	}
	$
}

score {
	staff { melodic music { alto }}

	commands { 
		meter {4*4} grouping {4*4}
	}
	paper { 
		unitspace 14mm
		geometric 1.4
	}
}
