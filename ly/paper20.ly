% paper20.ly


\version "1.2.0";

paper_twenty = \paper {
	staffheight = 20.0\pt;

	font_large = 12.;
	font_Large = 12.;	
	font_normal = 10.;
	font_script = 8.;

	font_finger = 5.;
	font_volta = 8.;
	font_number = 10.;
	font_mark = 12.;

	% what about:
	"font_number-1" = 8.;
	%"font_number" = 10.;
	"font_number+1" = 12.;
	
	% Ugh
	magnification_dynamic = 2.;
	
	% ugh see table20 for sizes
	quartwidth =  6.61\pt;
	wholewidth = 9.90\pt;

	-2 = \font "feta13"
	-1 = \font "feta16"
	0 = \font "feta20"

	"font_feta-2" = 13.;
	"font_feta-1" = 16.;
	"font_feta" = 20.;

	\include "params.ly";
}

\paper { \paper_twenty }
