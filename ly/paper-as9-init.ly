% paper-as9-init.ly

\version "1.7.18"

paperAsNine = \paper {
	staffheight = 9.\char

	%\translator { \StaffContext barSize = \staffheight }

	\stylesheet #(as-make-style-sheet 'as9)

	\include "params-as-init.ly"
	
}

\paper { \paperAsNine }
