% Toplevel initialisation file. 
	
\version "1.1.66";


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
