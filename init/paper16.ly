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
	% 2 interbeam + beam_thickness = 2 interline - staffline_thickness
	% ( beam_thickness = 0.48 interline for now...)
	% interbeam = interline - (beam_thickness + staffline_thickness) / 2
	% interbeam = 2.84;
	% ugh: interline *in fact* is rule_thickness + "interline"? --jcn

	% No --hwn
	interbeam = 3.14;

	gourlay_energybound = 100000.;
	gourlay_maxmeasures = 14.;
	castingalgorithm = \Gourlay;
	\symboltables { \table_sixteen }
	\include "engraver.ly"
}
