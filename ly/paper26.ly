% paper26.ly

\version "1.3.96";

paper_twentysix = \paper {
	staffheight = 26.0\pt;
	\stylesheet #(make-style-sheet 'paper26)	

	\include "params.ly";
}

\paper { \paper_twentysix }
