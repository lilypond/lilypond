% paper-musa.ly

\version "1.3.24";

paper_musa_nine = \paper {
	staffheight = 9.\char;

	font_large = 12.;
	font_Large = 12.;	
	font_normal = 10.;
	font_script = 8.;

	font_finger = 1.;
	font_volta = 1.;
	font_number = 1.;
	font_mark = 1.;

	% what about:
	"font_number-1" = 8.;
	%"font_number" = 10.;
	"font_number+1" = 12.;
	
	% Ugh
	magnification_dynamic = 2.;
	
	% ugh see table20 for sizes
	quartwidth =  3.\char;
	wholewidth = 3.\char;

	-2 = \font "musa9"
	-1 = \font "musa9"
	0 = \font "musa9"

	"font_feta-2" = 9.;
	"font_feta-1" = 9.;
	"font_feta" = 9.;

	\include "params-musa.ly";
}

\paper { \paper_musa_nine }
