% paper11-init.ly

\version "1.9.8"

paperEleven = \paper {
	staffheight = 11.0\pt
	#(define fonts (scale-font-list  (/ 11. 20.)))

	\include "params-init.ly"
}

\paper { \paperEleven }
