% paper11.ly

\version "1.3.120";

paperEleven = \paper {
	staffheight = 11.0\pt;
	\stylesheet #(make-style-sheet 'paper11)

	\include "params.ly";
}

\paper { \paperEleven }
