% paper16-init.ly

\version "1.5.49"

paperSixteen = \paper {
	staffheight = 16.0\pt
	\stylesheet #(make-style-sheet 'paper16)

	\include "params-init.ly"
}

\paper {\paperSixteen }
