%{MudelaHeader

 filename: collisions.ly
 title:
 description:  random counterpoint to test collisions
 composer(s): 
 entered-by: HWN,JCN
 copyright: public domain

 Tested Features:test the Collision resolution 
EndMudelaHeader
%}
\version "0.1.0";



two_voice = \melodic 
	< \multi 2; 
	  {	\octave c'; \stem \down;
		c4 d e f g2~  g4 a [c8 d e f] c2| }
	  { \stem \up;
		g4 f e g ~ g2 g2  c4 g4 g2 } 

	>

two_voice_steminvert = \melodic 
	< \multi 2;  
	  {	\octave c'; \stem \up;
% the f and g on 4th beat are exceptionally ugh.
		c4 d e f g2 g4 a | }
	  { \stem \down;
		g4 f e g  g2 g2 } 

	>

three_voice = \melodic 
	< \multi 2;
	{ \stem \up; 
		g4 f e f g a g2 }
	{ \hshift 1; \stem \up; 
		e2  e2  e2  e2 }
	{ \stem \down;
		c4 d e d c d es }
	>


rests = \melodic  
	< \multi 2;
	{ \stem \up;
		| r8 r r r  r r r r 
		[c' b a g] [f e d c]
	} 
	{ \stem \down;
		[c8 d e f] [g a b c']
		r r r r r r r r 
	}
	>

restsII = \melodic {
	\octave c'; 
			< \multi2;  
				{ \stem \up;  g' f' e' d' c' b a g f e d c }
				{ \stem \down; r  r  r  r  r  r r r r r r r }
			>
			< \multi2;  
				{ \stem \up;  r r r r r r r r  r  r  r  r }
				{ \stem \down; c d e f g a b c' d' e' f' g' }
			>
			r8 r4
			< \multi2;  r8 r8 >
			< \multi2;  r8 r8 r8 >
			< \multi2;  r8 r8 r8 r8 >
			< \multi2;  r r >
			< \multi2;  r r r >
			\stem \up;
			[c''8 r8 c''8 c''8]
			[c8 r8 c8 c8]
			\stem \down;
			[c8 r8 c8 c8]
			[c''8 r8 c''8 c''8]
}

\score{
	\melodic {  \$two_voice  \$two_voice_steminvert 
			\$three_voice  \rests \restsII }
	

	
%	\midi { \tempo 4:80 }
}
