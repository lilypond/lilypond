\header{
filename =	 "collisions.ly";
description =	 "random counterpoint to test collisions";
enteredby =	 "HWN,JCN";
copyright =	 "public domain";
Tested =	 "test the Collision resolution ";
}
\version "0.1.14";



two_voice = \melodic 
	\multi 2 < 
	  { \stemdown \octave c'; 
		c4 d e f g2~  g4 a [c8 d e f] c2| }
	  { \stemup
		g4 f e g ~ g2 g2  c4 g4 g2 } 

	>

two_voice_steminvert = \melodic 
	\multi 2 <  
	  { 
		\octave c'; \stemup
% the f and g on 4th beat are exceptionally ugh.
		c4 d e f g2 g4 a | }
	  { \stemdown
		g4 f e g  g2 g2 } 

	>

three_voice = \melodic 
	\multi 2 <
	{ \stemup 
		g4 f e f g a g2 }
	{ \stemup \property Voice.hshift = 1 
		e2  e2  e2  e2 }
	{ \stemdown
		c4 d e d c d es }
	>


rests = \melodic  
	\multi 2 <
	{ \stemup
		| r8 r r r  r r r r 
		[c' b a g] [f e d c]
	} 
	{ \stemdown
		[c8 d e f] [g a b c']
		r r r r r r r r 
	}
	>

restsII = \melodic {
	\octave c'; 
	\multi 2 < 
		{ \stemup  g' f' e' d' c' b a g f e d c }
		{ \stemdown r  r  r  r  r  r r r r r r r }
	>
	\multi 2 <
		{ \stemup  r r r r r r r r  r  r  r  r }
		{ \stemdown c d e f g a b c' d' e' f' g' }
	>
	r8 r4
	\multi 2 <  r8 r8 >
	\multi 2 <  r8 r8 r8 >
	\multi 2 <  r8 r8 r8 r8 >
	\multi 2 <  r r >
	\multi 2 <  r r r >
	\stemup
	[c''8 r8 c''8 c''8]
	[c8 r8 c8 c8]
	\stemdown
	[c8 r8 c8 c8]
	[c''8 r8 c''8 c''8]
}

\score{
	{  \two_voice  
	\two_voice_steminvert 
	\three_voice  
	\rests 
	% UGH ! bug!
	%\restsII 
	}
	

	
%	\midi { \tempo 4:80 }
}
