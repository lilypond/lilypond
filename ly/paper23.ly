% paper23.ly


\version "1.3.59";

paper_twentythree = \paper {
	staffheight = 23.0\pt;

	font_Large = 12.;	
	font_large = 12.;
	font_normal = 10.;
	font_script = 8.;

	font_finger = 5.;
	font_volta = 8.;
	font_number = 10.;
	font_mark = 12.;

	% Ugh
	magnification_dynamic = 3.;
	
	% ugh see table20 for sizes
	quartwidth =  6.61\pt;
	wholewidth = 9.90\pt;

	-2 = \font "feta16"
	-1 = \font "feta20"
	0 = \font "feta23"

	"font_feta-2" = 16.;
	"font_feta-1" = 20.;
	"font_feta" = 23.;

	\include "params.ly";
}

\paper { \paper_twentythree }
