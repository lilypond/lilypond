% paper16.ly

\include "table13.ly";
\include "table11.ly";
\include "table16.ly";



\version "1.0.7";

paper_sixteen = \paper {
	staffheight = 16.0\pt;

	% ugh, see table16 for sizes
	quartwidth = 5.28\pt;
	wholewidth = 7.92\pt;


     	arithmetic_basicspace = 2.;
        arithmetic_multiplier = 4.8\pt;
	texsetting = "\\input lilyponddefs \\musixsixteendefs ";
	pssetting = "(lilyponddefs.ps) findlibfile {exch pop //systemdict /run get exec} { /undefinedfilename signalerror } ifelse\n";

	0 = \symboltables { \table_sixteen }
	-1 = \symboltables { \table_thirteen }
	-2 = \symboltables { \table_eleven }
	
	\include "params.ly";
}
