% paper13.ly

\version "1.3.93";

paper_thirteen = \paper {
	staffheight = 13.0\pt;


	font_Large = 8.;
	font_large = 6.;
	font_normal = 5.;
	font_script = 4.;

	font_finger = 4.;
	font_volta = 4.;
	font_number = 6.;
	font_dynamic = 10.;
	font_mark = 6.;

	0=\font "feta13"
	-1=\font "feta11"

	"font_feta-2" = 11.;
	"font_feta-1" = 11.;
	"font_feta" = 13.;
	
	\include "params.ly";
}

\paper { \paper_thirteen }
