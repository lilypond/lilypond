% paper11.ly

\version "1.3.96";

paper_eleven = \paper {
	staffheight = 11.0\pt;
	style_sheet = "paper11";

	-1=\font "feta11"
	-2=\font "feta11"
	0=\font "feta11"

	\include "params.ly";
}

\paper { \paper_eleven }
