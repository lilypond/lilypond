% paper-as9.ly

\version "1.3.42";

paper_as_nine = \paper {
	staffheight = 9.\char;

	font_large = 4.;
	font_Large = 4.;	
	font_normal = 4.;
	font_script = 1.;

	font_finger = 1.;
	font_volta = 1.;
	font_number = 4.;
	font_mark = 4.;

	% what about:
	"font_number-1" = 1.;
	%"font_number" = 10.;
	"font_number+1" = 4.;
	
	% Ugh
	magnification_dynamic = 2.;
	
	% ugh see table20 for sizes
	quartwidth =  3.\char;
	wholewidth = 3.\char;

	-2 = \font "as9"
	-1 = \font "as9"
	0 = \font "as9"

	"font_feta-2" = 9.;
	"font_feta-1" = 9.;
	"font_feta" = 9.;

	\include "params-as.ly";
}

\paper { \paper_as_nine }
