% paper26.ly

\version "1.3.93";

paper_twentysix = \paper {
	staffheight = 26.0\pt;
	font_Large = 17.;	
	font_large = 14.;
	font_normal = 12.;
	font_script = 10.;

	font_dynamic = 10.;
	% Ugh
	magnification_dynamic = 4.;

	font_finger = 8.;
	font_volta = 10.;
	font_number = 10.;
	magnification_number = 2.;
	font_mark = 14.;

	0=\font "feta26"
	-1 = \font "feta23"
	-2 = \font "feta20"

	"font_feta-2" = 20.;
	"font_feta-1" = 23.;
	"font_feta" = 26.;

	\include "params.ly";
}

\paper { \paper_twentysix }
