\header{
filename =	 "collisions.ly";
description =	 "random counterpoint to test collisions";
enteredby =	 "HWN,JCN";
copyright =	 "public domain";
Tested =	 "test the Collision resolution ";
}
\version "1.0.7";



twovoice = \notes 
	\type Staff < 
	  { \stemdown
		c4 d e f g2~  g4 a [c8 d e f] c2| }
	  { \stemup
		g4 f e g ~ g2 g2  c4 g4 g2 } 

	>

twovoicesteminvert = \notes 
	\type Staff <  
	  { 
 \stemup
% the f and g on 4th beat are exceptionally ugh.
		c4 d e f g2 g4 a | }
	  { \stemdown
		g4 f e g  g2 g2 } 

	>

threevoice = \notes 
	\type Staff <
	{ \stemup 
		g4 f e f g a g2 }
	{ \stemup \property Voice.hshift = 1 
		e2  e2  e2  e2 }
	{ \stemdown
		c4 d e d c d es }
	>


rests = \notes  
	\type Staff <
	{ \stemup
		| r8 r r r  r r r r 
		[c' b a g] [f e d c]
	} 
	{ \stemdown
		[c8 d e f] [g a b c']
		r r r r r r r r 
	}
	>

restsII = \notes {
	\type Staff < 
		{ \stemup  g' f' e' d' c' b a g f e d c }
		{ \stemdown r  r  r  r  r  r r r r r r r }
	>
	\type Staff <
		{ \stemup  r r r r r r r r  r  r  r  r }
		{ \stemdown c d e f g a b c' d' e' f' g' }
	>
	r8 r4
	\type Staff <  r8 r8 >
	\type Staff <  r8 r8 r8 >
	\type Staff <  r8 r8 r8 r8 >
	\type Staff <  r r >
	\type Staff <  r r r >
	\stemup
	[c''8 r8 c''8 c''8]
	[c8 r8 c8 c8]
	\stemdown
	[c8 r8 c8 c8]
	[c''8 r8 c''8 c''8]
}

\score{
	\notes \transpose c' {  \twovoice  
	\twovoicesteminvert 
	\threevoice  
	\rests 
	% UGH ! bug!
	%\restsII 
	}
	

	
%	\midi { \tempo 4:80 }
}
