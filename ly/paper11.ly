% paper11.ly

\version "1.3.59";

paper_eleven = \paper {
	staffheight = 11.0\pt;


	font_Large = 8.;
	font_large = 6.;
	font_normal = 5.;
	font_script = 4.;

	font_finger = 4.;
	font_volta = 4.;
	font_number = 4.;
	font_dynamic = 10.;
	font_mark = 6.;

	% UGH!
	magnification_dynamic = -4.0;

	-1=\font "feta11"
	-2=\font "feta11"
	0=\font "feta11"

	"font_feta-2" = 11.;
	"font_feta-1" = 11.;
	"font_feta" = 11.;

	\include "params.ly";
}

\paper { \paper_eleven }
