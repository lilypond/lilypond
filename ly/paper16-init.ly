% paper16-init.ly


\version "1.9.8"

paperSixteen = \paper {
	staffheight = 16.0\pt
	#(define fonts (scale-font-list (/ 16. 20.)))
	\include "params-init.ly"
}

\paper { \paperSixteen }
