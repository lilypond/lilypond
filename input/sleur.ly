\header{
enteredby =	 "jcn";
copyright =	 "PD";
TestedFeatures =	 "This file tests Feta embedded slurs"
	 "(Feta definitively is not an abbreviation of Font-En-TjA)";
}

%{ remember to:
rm `find /var/lib/texmf -name "feta-sleur-*"`
%}

\version "0.1.7";

\score{
	\melodic{ 
		\octave c'
		;c'1() g' () c () g () c 
	}
	\paper{ }
}

