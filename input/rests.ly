%{MudelaHeader

 filename: rests.ly
 title: 
 description: 
 composer(s): heu
 entered-by: jcn
 copyright: GPL

 Tested Features: rest collisions
EndMudelaHeader
%}

\score{
	\staff{ \inputregister{melodicregs}
		\melodic{ 
			\meter 4/4;
			\octave c'; 
			< \multivoice 
				{ \stem 1;  g' f' e' d' c' b a g f e d c }
				{ \stem -1; r  r  r  r  r  r r r r r r r }
			>
			< \multivoice 
				{ \stem 1;  r r r r r r r r  r  r  r  r }
				{ \stem -1; c d e f g a b c' d' e' f' g' }
			>
			r8
			< \multivoice r8 r8 >
			< \multivoice r8 r8 r8 >
			< \multivoice r8 r8 r8 r8 >
			< \multivoice r r >
			< \multivoice r r r >
			\stem 1;
			[c''8 r8 c''8 c''8]
			[c8 r8 c8 c8]
			\stem -1;
			[c8 r8 c8 c8]
			[c''8 r8 c''8 c''8]
		}
	}
	\paper{}
}
