
\header{
 filename	rhythm.ly
 entered-by	HWN
 copyright	public domain
 Tested Features	 multiple meters, beaming
	unsynced bars
}
%{
has to be revised for 0.1.12
%}



\version "0.1.1";

ritme = \melodic{ %\octave ;
	\partial 8;
	\octave c';
	\meter  4/4;
	c8					|
	
	[a8~  a8. a8 a16 a16 a16] c4.		|	% watch the beams!
	 r32 r32 r16 r8 r4 r2			|
	\meter   5/16;

	% divide measure in 5 equal parts. Usually it 2+3 or 3+2
	\grouping  16*5 ;	
	[c8 c16 c8 ]				|	% watch THIS!
	 [5/4 c16 c16 c16 c16]1/1 |
	\meter   2/8;
	c4 				|
	c4	c4	c4	c4
	\meter 4/4;
	c1 c1 c1
	
%	[c16 c16 c16 c16 ]			|
%	[c16 c16 c16 c16 c16 ]			|
%	[c16 c16 c16 c16 c16 ]			|	
	
	 }
	

another = 
	\melodic{ \meter 4/4; 
		c1.  c1. c4 c4 c4 c4  \meter  4/4; c1 c1 c1
	 }


yanother = 
	\melodic{ \meter 4/4; 
		c1 c1 c1 c4 c4 c4 c4  c1 c1 c1
	 }


\score{
	 < \multi 3;
		\ritme
		\another
		\yanother
	>
	
	\paper{
	}
}
