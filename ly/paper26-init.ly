% paper26-init.ly

\version "1.9.8"

paperTwentysix = \paper {
	staffheight = 26.0\pt
	#(define fonts (scale-font-list (/ 26. 20.)))
	\include "params-init.ly"
}

\paper { \paperTwentysix }
