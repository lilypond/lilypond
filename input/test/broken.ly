\header{
enteredby =	 "jcn";
copyright =	 "PD";
TestedFeatures =	 "This file tests Feta embedded slurs" +
	 "(Feta definitively is not an abbreviation of Font-En-TjA)";
}

\version "1.0.7";

shortlong = \notes{
	c4()c( c c  |
	c c c c |
	c c c c |
	\break
	c c c )c |
}

broken = \notes\transpose c''{

      c c c c()
      c c c c(
      c )c c c(
      c c )c c(
      )a' a' a' a'()
      a' a' a' a'()
      c( c c )c 
      c( c c )f,
      f,( c c )c 
      f,( c c )f,
}

\score{
	\notes{ 
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

