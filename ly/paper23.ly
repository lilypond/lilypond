% paper23.ly


\version "1.3.146"

paperTwentythree = \paper {
	staffheight = 23.0\pt
	\stylesheet #(make-style-sheet 'paper23)
	\include "params.ly"
}

\paper { \paperTwentythree }
