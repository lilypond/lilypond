% paper20.ly


\version "1.3.146"

paperTwenty = \paper {
	staffheight = 20.0\pt
	\stylesheet #(make-style-sheet 'paper20)
	
	\include "params.ly"
}

\paper { \paperTwenty }
