% Toplevel initialisation file. 
	
\version "1.0.3";


\include "declarations.ly"

\include "paper16.ly";

default_paper = \paper { 
  \paper_sixteen
  linewidth = 7.\cm;
}

\score { 
%  \notes\relative c {
  \notes {
    \maininput
  }
  \paper { 
    linewidth = -1.0\cm;
    castingalgorithm = \Wordwrap;
  }
}
