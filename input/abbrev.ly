%{MudelaHeader

 filename: abbrev.ly
 title: 
 description: 
 composer(s): 
 entered-by: jcn
 copyright: PD

 Tested Features: simple abbreviations
EndMudelaHeader
%}

\version "0.1.5";

\score{
	\melodic{ 
		\octave c'';
		\meter 4/4;
		'a1
		'a1:32
		'c4:8 c' 'c4:16 c'
		[ 'c8:16 'c 'c 'c ] [ a a a a ]
		[ 'c 'f 'b e ] 
		[ 'c16:32 'c 'c 'c ] [ a16:32 a a a ]
%%		% there's still some hairy beam bugfixing todo
%%		[ c'8:16 g d 'a ]
%%		[ c'16:32 g d 'a ]
%%		[ 'c8:32 'f 'b e ]
		[:16 c4 e]
		[:16 e4 g]
		[:16 e2 g]
		[:16 e1 g]
	}
	\paper{ 
	}
% oeps
	\midi{ }
}

