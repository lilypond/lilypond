% paper16.ly

\include "table16.ly"


%{

TODO make this somehow parametric. for example the linewidth was
chosen to look good on A4 paper.  This probably looks less good on US
paper sizes.


%}
paper_sixteen = \paper {
	linewidth = 15.5 \cm;
	indent = 10.\mm;
	rulethickness = 0.4\pt;
	barsize = 16.0 \pt;
	interline = 4.\pt;
	notewidth = 5.93\pt; % ugh, see table16 for sizes
	wholewidth = 8.64\pt;
	unitspace = 22.\pt;
	%geometric = 1.414;
	basicspace = 4.\pt;

	geometric = 0.;
     	arithmetic_basicspace = 2.;
        arithmetic_multiplier = 4.8\pt;
   
	% three beams span two interlines, including stafflines:
	% 2ib + bt = 2 il - st
	% bt = 0.48(il - st) for now.
	% 2ib + 0.48il - 0.48 st = 2il - st
	% 2ib = 1.52il - 0.52 st
	% ib = 0.76il - 0.26st = 2.94 --jcn
	% now, it seams rather ib = 0.76il + 0.26st = 3.14
	% interbeam = 2.94\pt;
	% No -- hwn
	interbeam = 3.14\pt;

	gourlay_energybound = 100000.;
	%{
	The following bounds the number of measures
	on a line.  Decreasing it greatly reduces computation time
	%}
	gourlay_maxmeasures = 10.;
	castingalgorithm = \Gourlay;
	\symboltables { \table_sixteen }
	\include "engraver.ly"
}
