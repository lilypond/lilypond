% paper20.ly

\include "table20.ly"

paper_twenty = \paper {
	linewidth = 15.0 \cm;
	indent = 12.\mm;
	rulethickness = 0.4\pt;
	barsize = 20.0 \pt;
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
	
	% three beams span two interlines, including stafflines:
	% 2ib + bt = 2 il - st
	% bt = 0.48(il - st) for now.
	% 2ib + 0.48il - 0.48 st = 2il - st
	% 2ib = 1.52il - 0.52 st
	% ib = 0.76il - 0.26st = 3.70
	% now, it seams rather ib = 0.76il + 0.26st = 3.90
	% interbeam = 3.70\pt;
	interbeam = 3.90\pt;

	gourlay_energybound = 100000.;
	gourlay_maxmeasures = 12.;
	
	castingalgorithm = \Gourlay;
	\symboltables { \table_twenty }
\include "engraver.ly"
}

