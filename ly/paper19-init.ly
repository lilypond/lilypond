% paper19-init.ly


\version "1.9.8"

paperNineteen = \paper {
	staffheight = 18.0\pt
	#(define fonts (scale-font-list  (/ 18.0 20.0) ))

	\include "params-init.ly"
}

\paper { \paperNineteen }

