% paper26.ly

\include "table26.ly";

paper_twentysix = \paper {
	staffheight = 26.0\pt;

	% ugh see table26 for sizes
	quartwidth = 8.59\pt;
	wholewidth = 12.87\pt;

	arithmetic_basicspace = 2.;
        arithmetic_multiplier = 6.\pt;
	
	0=\symboltables { \table_twentysix }
	\include "params.ly";
	linewidth = \linewidth20;
	textheight = \textheight20;
}

