\header{
title= "Stems and Beams";
subtitle = "proofsheet"; 
enteredby =	 "jcn";
copyright =	 "public domain";
TestedFeatures =	 "This file tests the length of stems and placement 
of beams";
	
}

\version "0.1.8";

beamintervals = \melodic{
		\meter 7/4;
		\stemup;
		\octave c';
		[ c8 d ] [ c e ] [ c f ] [ c g ] [ c a ] [ c b ] [ c c' ] |
		[ c 'b ] [ c 'a ] [ c 'g ] [ c 'f ] [ c 'e ] [ c 'd ] [ c 'c ] |
		\octave c'';
		[ c 'b ] [ c 'a ] [ c 'g ] [ c 'f ] [ c 'e ] [ c 'd ] [ c 'c ] |
		\stemdown;
		\octave c''';
		[ c 'b ] [ c 'a ] [ c 'g ] [ c 'f ] [ c 'e ] [ c 'd ] [ c 'c ] |
		\octave c'';
		[ c 'b ] [ c 'a ] [ c 'g ] [ c 'f ] [ c 'e ] [ c 'd ] [ c 'c ] |
		[ c d ] [ c e ] [ c f ] [ c g ] [ c a ] [ c b ] [ c c' ] |
	}

\score{
	\melodic{ 
		\octave c';
		\stemup;
		\meter 17/4;
		'g4 'a 'b c d e f g a b c' d' e' f' g' a' b' |
		\stemdown
		b' a' g' f' e' d' c' b a g f e d c 'b 'a 'g |
		\beamintervals;
		\transpose d \beamintervals;
		\transpose e \beamintervals;
		\transpose f \beamintervals;
		\transpose g \beamintervals;
		\transpose a \beamintervals;
		\transpose b \beamintervals;
	}
	\paper{
		gourlay_maxmeasures = 2.;
	}
}
