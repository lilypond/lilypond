% paper23-init.ly


\version "1.5.68"

paperTwentythree = \paper {
	staffheight = 23.0\pt
	#(define fonts (make-font-list 'paper23))
	\include "params-init.ly"
}

\paper { \paperTwentythree }
