% paper13-init.ly

\version "1.3.146"

paperThirteen = \paper {
	staffheight = 13.0\pt

	\stylesheet #(make-style-sheet 'paper13)
	
	\include "params-init.ly"
}

\paper { \paperThirteen }
