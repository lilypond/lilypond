% paper11.ly


paper_eleven = \paper {
	staffheight = 11.0\pt;

        % ugh see table11 for sizes
	quartwidth = 3.63 \pt;
	wholewidth = 5.45 \pt;

	font_large = 8.;
	font_Large = 6.;
	font_normal = 5.;

	font_finger = 4.;
	font_volta = 4.;
	font_number = 4.;
	font_dynamic = 10.;
	font_mark = 6.;
	magnification_dynamic = -4.0;
	-1=\font "feta11"
	-2=\font "feta11"
	0=\font "feta11"

	\include "params.ly";
}

\paper { \paper_eleven }
