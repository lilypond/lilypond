% Toplevel initialisation file. 
	
\version "1.0.6";


\include "declarations.ly"

\include "paper16.ly"

\score { 
  \notes {
    \maininput
  }
  \paper { \paper_sixteen
    linewidth = -1.\cm;
    castingalgorithm = \Wordwrap; 
  }
}
