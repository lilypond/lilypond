% paper20.ly

\include "table20.ly";

 \include "table13.ly";
 \include "table16.ly";

\version "1.0.7";

paper_twenty = \paper {
	staffheight = 20.0\pt;

	% ugh see table20 for sizes
	quartwidth =  6.61\pt;
	wholewidth = 9.90\pt;


	arithmetic_basicspace = 2.;
        arithmetic_multiplier = 6.\pt;
	texsetting = "\\input lilyponddefs \\musixtwentydefs ";
	pssetting = "(lilyponddefs.ps) findlibfile {exch pop //systemdict /run get exec} { /undefinedfilename signalerror } ifelse\n";

	-2 = \symboltables { \table_thirteen }	
	-1 = \symboltables { \table_sixteen }
	0 = \symboltables { \table_twenty }
	
	\include "params.ly";
}

