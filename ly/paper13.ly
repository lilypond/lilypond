% paper13.ly

\version "1.3.96";

paper_thirteen = \paper {
	staffheight = 13.0\pt;
	style_sheet = "paper13";

	0=\font "feta13"
	-1=\font "feta11"

	\include "params.ly";
}

\paper { \paper_thirteen }
