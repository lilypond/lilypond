% The purpose of this file is to demonstrate features of LilyPond; 
%
% COPYRIGHT: GPL
%
%
\version "0.0.54";

globals=\melodic{
		\meter 4/ 4;
		\partial 8;
		\skip 8*1;
		\skip 2*3 ;
		\bar ":|:";
		\skip 1*2;
		\meter 2/4;
}

%ritme = \staff{melodicregs
% 	globals
% rhythmic broken for  now
% 	\rhytmic{
% 	c8
% 	|[a8() a8. a8 a16 a16 a16] c4.
% 		
% 	%[c8( )a'8() c8 c8]% BUG!
% 	|c2 c2
% 	
% 	|[fis16 dis'16( fis16 dis'16 ][fis16) dis'16 fis16 dis'16]
% 	| r32 r32 r16 r8 r4 r2
% 	|[c8. c16] [c16 c8.] [c16 c16 c8] [c16 c8 c16]
% 
% 	 c2 c2 c2 c2 c2 c2 c2 c2 c2 c2
% 	}
%}

melody= \staff{melodicregs
	globals
	\melodic {
	\octave  c;
	c8\key fis cis gis;
	|r4 r4 r4 r4
	|cis'2..	r8
	| r4 r8 r16 r32 r32 

	\duration 4;
	<
		 { c () 'bes [c8 c8] }
		 { fis' ()gisis' fis8 fis8 }
		 { d () d dis8 dis8 }
		 {  a  () bes eis8 eis8 }
		 { fis () g gis8 gis8 }
	>
	\clef\tenor;
	< c4 c'4 >

	[d8 e8 f'8 g8]  d8 e8 f8 g8
	|fis''2
	| a8 b8 c'8 d'8 |c''8 '''c8 c4 |c4  c4 |c4
	\duration  16 ; 'b 'a 'g 'f \duration  4;
	\clef\bass	;

	|c 'b 'a 'g 'f 'e 'd 'c ''b ''a ''g ''f ''e ''d ''c
	}

}

\score{
	\paper{
		\geometric 1.4
		\unitspace 3.0 \cm
	}

	\staff{	 melody}
}

