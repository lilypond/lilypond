% paper26.ly

\include "table26.ly";

paper_twentysix = \paper {
	staffheight = 26.0\pt;
	papersize = "a4";

	% ugh see table26 for sizes
	notewidth = 8.59\pt
	wholewidth = 12.87\pt;

	basicspace = 8.\pt;
	arithmetic_basicspace = 2.;
        arithmetic_multiplier = 6.\pt;
	
	0=\symboltables { \table_twentysix }
	\include "params.ly";
	linewidth = linewidth20;
	textheight = textheight20;
}

