% Toplevel initialisation file. 
	
\version "0.1.15";


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
