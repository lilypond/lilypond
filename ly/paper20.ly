% paper20.ly


\version "1.3.96";

paper_twenty = \paper {
	staffheight = 20.0\pt;
	style_sheet = "paper20";

	0 = \font "feta20"
	-1 = \font "feta16"
	-2 = \font "feta13"
	-3 = \font "feta11"

	\include "params.ly";
}

\paper { \paper_twenty }
