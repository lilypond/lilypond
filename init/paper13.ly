% paper20.ly

\include "table13.ly"

paper_thirteen = \paper {
	linewidth = 15.0 \cm;
	indent = 8.0\mm;
	rulethickness = 0.25\pt;
	barsize = 13.0 \pt;
	interline = 3.25\pt;
	notewidth = 7.15\pt; % ugh see table20 for sizes
	wholewidth = 10.44\pt;
	unitspace = 22.\pt;
%	basicspace = 4.\pt;
%	geometric = 1.414;
	geometric = 0.;
	basicspace = 9.\pt;

	arithmetic_basicspace = 2.;
        arithmetic_multiplier = 7.\pt;
	
	%
	% three beams span two interlines, including stafflines:
	% 2 interbeam + beam_thickness = 2 interline - staffline_thickness
	% ( beam_thickness = 0.48 interline for now...)
	% interbeam = interline - (beam_thickness + staffline_thickness) / 2
	% interbeam = 3.6;
	% ugh: interline *in fact* is rule_thickness + "interline"?
	interbeam = 3.9;

	gourlay_energybound = 100000.;
	gourlay_maxmeasures = 12.;
	
	castingalgorithm = \Gourlay;
	\symboltables { \table_thirteen }
	\include "engraver.ly"
}

