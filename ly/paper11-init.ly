% paper11-init.ly

\version "1.5.49"

paperEleven = \paper {
	staffheight = 11.0\pt
	\stylesheet #(make-style-sheet 'paper11)

	\include "params-init.ly"
}

\paper { \paperEleven }
