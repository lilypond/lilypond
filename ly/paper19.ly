% paper20-init.ly


\version "1.5.49"

paperNineteen = \paper {
	staffheight = 19.0\pt
	\stylesheet #(make-style-sheet 'paper19)
	
	\include "params-init.ly"
}

\paper { \paperNineteen }

