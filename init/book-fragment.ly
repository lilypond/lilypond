% Toplevel initialisation file. 
	
\version "0.1.10";


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
