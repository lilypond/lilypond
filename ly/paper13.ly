% paper13.ly

\version "1.3.110";

paper_thirteen = \paper {
	staffheight = 13.0\pt;

	\stylesheet #(make-style-sheet 'paper13)
	
	\include "params.ly";
}

\paper { \paper_thirteen }
