\header{
enteredby =	 "jcn";
copyright =	 "PD";
TestedFeatures =	 "This file tests Feta embedded slurs"
	 "(Feta definitively is not an abbreviation of Font-En-TjA)";
}

\version "0.1.15";

shortlong = \melodic{
	c4()c( c c  |
	c c c c |
	c c c c |
	\break;
	c c c )c |
}

broken = \melodic{
      \octave c'';
      c c c c()
      c c c c(
      c )c c c(
      c c )c c(
      )a' a' a' a'()
      a' a' a' a'()
      c( c c )c 
      c( c c )'f 
      f,( c c )c 
      f,( c c )'f
}

\score{
	\melodic{ 
%		\shortlong
		\broken
	}
	\paper{ 
	      indent = 0.0\pt;
		%for broken!
		linewidth= 30.\mm;
		castingalgorithm = \Wordwrap;
	}
}

