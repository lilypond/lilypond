% paper11.ly

\version "1.3.96";

paper_eleven = \paper {
	staffheight = 11.0\pt;
	\stylesheet #(make-style-sheet 'paper11)

	\include "params.ly";
}

\paper { \paper_eleven }
