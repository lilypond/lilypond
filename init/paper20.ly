% paper20.ly

\include "table20.ly"

paper_twenty = \paper {
	linewidth = 15.0 \cm;
	rule_thickness = 0.4\pt;
	bar_size = 20.0 \pt;
	interline = 5.\pt;
	notewidth = 7.15\pt; % ugh see table20 for sizes
	wholewidth = 10.44\pt;
	unitspace = 22.\pt;
%	basicspace = 4.\pt;
%	geometric = 1.414;
	geometric = 0.;
	basicspace = 8.\pt;

	arithmetic_basicspace = 2.;
        arithmetic_multiplier = 6.\pt;
	
	%
	interbeam = 3.333\pt;
	gourlay_energybound = 100000.;
	gourlay_maxmeasures = 12.;
	
	castingalgorithm = \Gourlay;
	\symboltables { \table_twenty }
\include "engraver.ly"
}

