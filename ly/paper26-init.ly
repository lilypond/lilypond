% paper26-init.ly

\version "1.5.49"

paperTwentysix = \paper {
	staffheight = 26.0\pt
	\stylesheet #(make-style-sheet 'paper26)	

	\include "params-init.ly"
}

\paper { \paperTwentysix }
