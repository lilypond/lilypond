% test the Collision resolution 
\version "0.0.56";

two_voice_steminvert = \melodic { 
	< \multivoice 
	  {	\octave c'; \stem 1;
		c d e f() g2 g a }
	  { \stem -1;
		g f e g ()g2 g2 } 

	>
}	

two_voice = \melodic { 
	< \multivoice 
	  {	\octave c'; \stem -1;
		c d e f g2() g a }
	  { \stem 1;
		g f e g ()g2 g2 } 

	>
}	

three_voice = \melodic {
	< \multivoice
	{ \stem 1; 
		g f e f g a g2 }
	{ \hshift 1; \stem 1; 
		e2  e2  e2  e2 }
	{ \stem -1;
		c d e d c d es }
	>
}

rests = \melodic  {
	< \multivoice
	{ \stem 1;\duration "last";
		r8 r r r  r r r r 
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
