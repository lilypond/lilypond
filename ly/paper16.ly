% paper16.ly

\version "1.3.110";

paper_sixteen = \paper {
	staffheight = 16.0\pt;
	\stylesheet #(make-style-sheet 'paper16)

	\include "params.ly";
}

\paper {\paper_sixteen }
