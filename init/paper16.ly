
\include "table16.ly"
\include "engraver.ly"

Gourlay = 1.0
Wordwrap = 0.0

default_paper = \paper {
	linewidth = 15.0 \cm;
	rule_thickness = 0.4\pt;
	bar_size = 16.0 \pt;
	interline = 4.\pt;
	notewidth = 5.0\pt;
	wholewidth = 4.8\pt;
	unitspace = 22.\pt;
	basicspace = 4.\pt;

	%
	interbeam = 2.667\pt;
	geometric = 1.414;
	gourlay_energybound = 50000.;
	gourlay_maxmeasures = 6.;
	castingalgorithm = \Gourlay;
	\symboltables { \table_sixteen }
	\requesttranslator { \orchestral_score_translator }
}
