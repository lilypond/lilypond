\header{
title= "Stems and Beams";
subtitle = "proofsheet"; 
enteredby =	 "jcn";
copyright =	 "public domain";
TestedFeatures =	 "This file tests the length of stems and placement 
of beams";
	
}

\version "1.0.7";

beamintervals = \notes{
		\time 7/4;
		\stemup
\transpose c'{
		[ c8 d ] [ c e ] [ c f ] [ c g ] [ c a ] [ c b ] [ c c' ] |
		[ c b, ] [ c a, ] [ c g, ] [ c f, ] [ c e, ] [ c d, ] [ c c, ] |
		}\transpose c''{
		[ c b, ] [ c a, ] [ c g, ] [ c f, ] [ c e, ] [ c d, ] [ c c, ] |
		\stemdown}
		\transpose c'''{
		[ c b, ] [ c a, ] [ c g, ] [ c f, ] [ c e, ] [ c d, ] [ c c, ] |
}		\transpose c''{
		[ c b, ] [ c a, ] [ c g, ] [ c f, ] [ c e, ] [ c d, ] [ c c, ] |
		[ c d ] [ c e ] [ c f ] [ c g ] [ c a ] [ c b ] [ c c' ] |
	}}

\score{
	\notes\transpose c'{ 
		\stemup
		\time 17/4;
		g,4 a, b, c d e f g a b c' d' e' f' g' a' b' |
		\stemdown
		b' a' g' f' e' d' c' b a g f e d c b, a, g, |
		\beamintervals
		\transpose d \beamintervals
		\transpose e \beamintervals
		\transpose f \beamintervals
		\transpose g \beamintervals
		\transpose a \beamintervals
		\transpose b \beamintervals
	}
	\paper{
		gourlay_maxmeasures = 2.;
	}
}
