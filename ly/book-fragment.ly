% Toplevel initialisation file. 
	
\version "1.3.110";


\include "declarations.ly"

\include "paper16.ly"
  \paper { \paper_sixteen
    linewidth = -1.\cm;
    castingalgorithm = \Wordwrap; 
    "unusedentry" = "}\\def\\nolilyfooter{";
  }

\score { 
  \notes {
    \maininput
  }
\paper{}
}
