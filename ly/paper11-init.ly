% paper11-init.ly

\version "1.9.7"

paperEleven = \paper {
	staffheight = 11.0\pt
	#(define fonts (make-font-list 'paper11))

	\include "params-init.ly"
}

\paper { \paperEleven }
