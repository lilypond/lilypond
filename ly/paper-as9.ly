% paper-as9.ly

\version "1.3.120";

paperAsNine = \paper {
	staffheight = 9.\char;

	%\translator { \StaffContext barSize = \staffheight; }

	\stylesheet #(as-make-style-sheet 'as9)

	\include "params-as.ly";
	
}

\paper { \paperAsNine }
