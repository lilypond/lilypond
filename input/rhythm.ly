%
% 	Ive' got rhythm!
%
% add any impressive examples here, please

ritme = \staff {
	\rhythmic{ 
	\partial {1*8}	% doesnt' have to be here. 
		\meter{ 4/4}
	c8					|
	
	[a8() a8. a8 a16 a16 a16] c4.		|	% watch the beams!
	 r32 r32 r16 r8 r4 r2			|
	\meter{  5/16}

	% divide measure in 5 equal parts. Usually it 2+3 or 3+2
	\grouping { 5*16 }	
	[c8 c16 c8 ]				|	% watch THIS!
	\plet{5/4} [c16 c16 c16 c16]\plet{1/1} |
	\meter{  2/8}
	c4 				|
	c4	c4	c4	c4
	\meter {4/4}
	c1 c1 c1
	
%	[c16 c16 c16 c16 ]			|
%	[c16 c16 c16 c16 c16 ]			|
%	[c16 c16 c16 c16 c16 ]			|	
	
	 }

	%
	% The \co\mmands section takes the same stuff that \music { } takes;
	% the \co\mmands issued below could have been issued inside the above
	% \music block;
	%

	
}
another = \staff{
	\melodic{ \meter{ 4/4} 
		c1 c1 c1 c4 c4 c4 c4  \meter{ 4/4} c1 c1 c1
	 }
}

yanother = \staff{
	\melodic{ \meter{ 4/4} 
		c1 c1 c1 c4 c4 c4 c4  c1 c1 c1
	 }
}

\score{
	ritme
	another
	yanother
	
	\paper{
		\unitspace 2\cm
		\geometric 1.3
	}
}
