% paper13.ly

paper_thirteen = \paper {
	staffheight = 13.0\pt;

	% ugh see table13 for sizes
	quartwidth = 4.29\pt;
	wholewidth = 6.44\pt;


	font_large = 8.;
	font_Large = 6.;
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
