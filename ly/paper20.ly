% paper20.ly


\version "1.3.110";

paper_twenty = \paper {
	staffheight = 20.0\pt;
	\stylesheet #(make-style-sheet 'paper20)
	
	\include "params.ly";
}

\paper { \paper_twenty }
