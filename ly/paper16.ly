% paper16.ly



\version "1.3.93";

paper_sixteen = \paper {
	staffheight = 16.0\pt;
	style_sheet = "paper20";

	0 = \font "feta16" 
	-1 = \font "feta13"
	-2 = \font "feta11"
	-3 = \font "feta11"

	\include "params.ly";
}

\paper {\paper_sixteen }
