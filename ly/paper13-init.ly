% paper13-init.ly


\version "1.9.8"

paperThirteen = \paper {
	staffheight = 13.0\pt
	#(define fonts (scale-font-list  (/ 13. 20.)))
	
	\include "params-init.ly"
}

\paper { \paperThirteen }
