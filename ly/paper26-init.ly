% paper26-init.ly

\version "1.5.68"

paperTwentysix = \paper {
	staffheight = 26.0\pt
	#(define fonts (make-font-list 'paper26))
	\include "params-init.ly"
}

\paper { \paperTwentysix }
