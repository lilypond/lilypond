%{MudelaHeader

 filename: collisions.ly
 title:
 description:  random counterpoint to test collisions
 composer(s): HWN
 entered-by: HWN
 copyright: public domain

 Tested Features:test the Collision resolution 
EndMudelaHeader
%}
\version "0.0.57";



two_voice = \melodic { 
	< \multivoice 
	  {	\octave c'; \stem -1;\duration "last";
		c4 d e f g2~  g4 a [c8 d e f] c2| }
	  { \stem 1;
		g4 f e g ~ g2 g2  c4 g4 g2 } 

	>
}	

two_voice_steminvert = \melodic { 
	< \multivoice  
	  {	\octave c'; \stem 1;
% the f and g on 4th beat are exceptionally ugh.
		c4 d e f g2 g4 a | }
	  { \stem -1;
		g4 f e g  g2 g2 } 

	>
}	
three_voice = \melodic {
	< \multivoice
	{ \stem 1; 
		g4 f e f g a g2 }
	{ \hshift 1; \stem 1; 
		e2  e2  e2  e2 }
	{ \stem -1;
		c4 d e d c d es }
	>
}

rests = \melodic  {
	< \multivoice
	{ \stem 1;
		| r8 r r r  r r r r 
		[c' b a g] [f e d c]
	} 
	{ \stem -1;
		[c8 d e f] [g a b c']
		r r r r r r r r r 
	}
	>
}
\score{
	\staff{ melodicregs 
		\melodic {  \$two_voice ++  \$two_voice_steminvert 
			++ \$three_voice ++ \rests
		}
	}
	\paper {}
%	\midi { \tempo 4:80 }
}
