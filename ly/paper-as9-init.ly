% paper-as9-init.ly

\version "1.9.0"

paperAsNine = \paper {
	staffheight = 9.\char

	%\translator { \StaffContext barSize = \staffheight }

	#(define fonts (as-make-font-list 'as9))

	\include "params-as-init.ly"
}

\paper { \paperAsNine }
