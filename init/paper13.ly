% paper13.ly

\include "table13.ly";

paper_thirteen = \paper {
	staffheight = 13.0\pt;
	linewidth = 15.0 \cm;

	% ugh see table13 for sizes
	quartwidth = 4.29\pt;
	wholewidth = 6.44\pt;

	basicspace = 3.\pt;
	arithmetic_basicspace = 2.;
        arithmetic_multiplier = 4.8\pt;
	
	\symboltables { \table_thirteen }
	\include "params.ly";
}

