% paper26.ly

\version "1.3.96";

paper_twentysix = \paper {
	staffheight = 26.0\pt;
	style_sheet = "paper26";

	0=\font "feta26"
	-1 = \font "feta23"
	-2 = \font "feta20"

	\include "params.ly";
}

\paper { \paper_twentysix }
