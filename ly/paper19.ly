% paper20-init.ly


#(error "Sorry, but support for paper19 has been (temporarily) suspended.
  (sorry)")

\version "1.5.68"

paperNineteen = \paper {
	staffheight = 19.0\pt
	\stylesheet #(make-style-sheet 'paper19)
	
	\include "params-init.ly"
}

\paper { \paperNineteen }

