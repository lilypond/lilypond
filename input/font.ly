\header{
enteredby	jcn
copyright	PD
TestedFeatures	font-en-tja
}

\version "0.1.6";

\score{
	\melodic{ 
		\octave c';
		\meter 4/4;
		c1 g c' a'
		c2 g c' a'
		c4 g c' a'
		a\ppp a\pp a\p a\mp a\mf a\f a\ff a\fff
		%a\fp a\sf a\sfz
	}
	\paper{ 
	    \paper_twenty
	    gourlay_maxmeasures =5.;
	    \output "lelie20.tex";
	}
	\paper{ 
	    gourlay_maxmeasures =5.;
	}
% oeps
	\midi{ }
}

