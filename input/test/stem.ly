\version "1.5.68"
\header{
title= "Stems and Beams"
subtitle =  "proofsheet" 
enteredby = 	 "jcn"
copyright = 	 "public domain"
TestedFeatures = 	 "This file tests the length of stems and placement 
of beams"
	
}



beamintervals =  \notes{
		\time 7/4
		\stemUp
\transpose c'{
		[ c8 d ] [ c e ] [ c f ] [ c g ] [ c a ] [ c b ] [ c c' ] |
		[ c b, ] [ c a, ] [ c g, ] [ c f, ] [ c e, ] [ c d, ] [ c c, ] |
		}\transpose c''{
		[ c b, ] [ c a, ] [ c g, ] [ c f, ] [ c e, ] [ c d, ] [ c c, ] |
		\stemDown}
		\transpose c'''{
		[ c b, ] [ c a, ] [ c g, ] [ c f, ] [ c e, ] [ c d, ] [ c c, ] |
}		\transpose c''{
		[ c b, ] [ c a, ] [ c g, ] [ c f, ] [ c e, ] [ c d, ] [ c c, ] |
		[ c d ] [ c e ] [ c f ] [ c g ] [ c a ] [ c b ] [ c c' ] |
	}}

\score{
	\notes\transpose c'{ 
		\stemUp
		\time 17/4
		g,4 a, b, c d e f g a b c' d' e' f' g' a' b' |
		\stemDown
		b' a' g' f' e' d' c' b a g f e d c b, a, g, |
		\beamintervals
		\transpose d \beamintervals
		\transpose e \beamintervals
		\transpose f \beamintervals
		\transpose g \beamintervals
		\transpose a \beamintervals
		\transpose b \beamintervals
	}
}
