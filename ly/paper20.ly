% paper20.ly


\version "1.3.59";

paper_twenty = \paper {
	staffheight = 20.0\pt;

	font_Large = 12.;	
	font_large = 12.;
	font_normal = 10.;
	font_script = 8.;

	font_finger = 5.;
	font_volta = 8.;
	font_number = 10.;
	font_timesig = 10.;
	font_mark = 12.;
	font_msam = 9.;

	% what about:
	"font_number-1" = 8.;
	%"font_number" = 10.;
	"font_number+1" = 12.;
	
	% Ugh
	magnification_dynamic = 2.;
	
	0 = \font "feta20"
	-1 = \font "feta16"
	-2 = \font "feta13"
	-3 = \font "feta11"

	"font_feta" = 20.;
	"font_feta-1" = 16.;
	"font_feta-2" = 13.;
	"font_feta-3" = 11.;

	\include "params.ly";
}

\paper { \paper_twenty }
