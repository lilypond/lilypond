% paper20.ly

\include "table20.ly"

Gourlay = 1.0
Wordwrap = 0.0

default_paper = \paper {
	linewidth = 15.0 \cm;
	rule_thickness = 0.4\pt;
	bar_size = 20.0 \pt;
	interline = 5.\pt;
	notewidth = 6.0\pt;
	wholewidth = 4.8\pt;
	unitspace = 22.\pt;
%	basicspace = 4.\pt;
%	geometric = 1.414;
	geometric = 0.;
	
     	arithmetic_basicspace = 2.5;
        arithmetic_multiplier = 4.8\pt;

	%
	interbeam = 2.667\pt;
	gourlay_energybound = 100000.;
	gourlay_maxmeasures = 8.;
	
	castingalgorithm = \Gourlay;
	\symboltables { \table_twenty }
	\requesttranslator { \orchestral_score_translator }
}
