\header{
enteredby =	 "jcn";
copyright =	 "PD";
TestedFeatures =	 "This file tests Feta embedded slurs"
	 "(Feta definitively is not an abbreviation of Font-En-TjA)";
}

\version "1.0.0";

shortlong = \melodic\transpose c'{
	c4()c( c c  |
	c c c c |
	c c c c |
	\break
	c c c )c |
}

sl = \melodic\transpose c''{

%	e4()e( a e  |
	e4 e( a e  |
	e e e e |
	e e e a |
	\break
	e e g )e |
}

broken = \melodic\transpose  c'' {
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
	\melodic{ 
		\shortlong
		\sl
		\broken
	}
	\paper{ 
	      indent = 0.0\pt;
		%for broken!
%		linewidth= 30.\mm;
		castingalgorithm = \Wordwrap;
	}
}

