% Toplevel initialisation file. 
	
\version "1.0.1";


\include "declarations.ly"

\include "paper16.ly";

default_paper = \paper { 
  \paper_sixteen
  linewidth = 7.\cm;
}

\score { 
%  \melodic\relative c {
  \melodic {
    \maininput
  }
  \paper { 
    linewidth = -1.0\cm;
    castingalgorithm = \Wordwrap;
  }
}
