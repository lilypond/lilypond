% paper26.ly

\version "1.3.146"

paperTwentysix = \paper {
	staffheight = 26.0\pt
	\stylesheet #(make-style-sheet 'paper26)	

	\include "params.ly"
}

\paper { \paperTwentysix }
