% Toplevel initialisation file. 
	
\version "1.0.0";


\include "declarations.ly"

\include "paper16.ly"

\score { 
  \melodic {
    \maininput
  }
  \paper { \paper_sixteen
    linewidth = -1.\cm;
    castingalgorithm = \Wordwrap; 
  }
}
