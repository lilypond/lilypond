% paper23.ly


\version "1.3.96";

paper_twentythree = \paper {
	staffheight = 23.0\pt;
	\stylesheet #(make-style-sheet 'paper23)
	\include "params.ly";
}

\paper { \paper_twentythree }
