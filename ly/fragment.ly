% Toplevel initialisation file. 
	
\version "1.3.120";


\include "declarations.ly"

\include "paper16.ly";

\paper { 
  \paperSixteen
    linewidth = -1.0\cm;
    castingalgorithm = \Wordwrap;
    "unusedentry" = "}\\def\\nolilyfooter{";
}


\score { 
%  \notes\relative c {
  \notes {
    \maininput
  }
  \paper { }

}
