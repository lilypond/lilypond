% paper23-init.ly


\version "1.9.8"

paperTwentythree = \paper {
	staffheight = 23.0\pt
	#(define fonts (scale-font-list  (/ 23. 20.)))
	\include "params-init.ly"
}

\paper { \paperTwentythree }
