% paper16.ly

\include "table16.ly";


%{

TODO make this somehow parametric. for example the linewidth was
chosen to look good on A4 paper.  This probably looks less good on US
paper sizes.

%}

paper_sixteen = \paper {
	staffheight = 16.0\pt;
	linewidth = 15.5 \cm;

	% ugh, see table16 for sizes
	quartwidth = 5.28\pt;
	wholewidth = 7.92\pt;

	basicspace = 4.\pt;
     	arithmetic_basicspace = 2.;
        arithmetic_multiplier = 4.8\pt;
   
	\symboltables { \table_sixteen }
	\include "params.ly";
}
