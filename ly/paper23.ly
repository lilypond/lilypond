% paper23.ly


\version "1.3.96";

paper_twentythree = \paper {
	staffheight = 23.0\pt;
	style_sheet = "paper23";

	-2 = \font "feta16"
	-1 = \font "feta20"
	0 = \font "feta23"

	\include "params.ly";
}

\paper { \paper_twentythree }
