% paper11-init.ly

\version "1.3.146"

paperEleven = \paper {
	staffheight = 11.0\pt
	\stylesheet #(make-style-sheet 'paper11)

	\include "params-init.ly"
}

\paper { \paperEleven }
