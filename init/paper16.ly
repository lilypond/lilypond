% paper16.ly

\include "table16.ly"


paper_sixteen = \paper {
	linewidth = 15.0 \cm;
	rule_thickness = 0.4\pt;
	bar_size = 16.0 \pt;
	interline = 4.\pt;
	notewidth = 5.0\pt;
	wholewidth = 4.8\pt;
	unitspace = 22.\pt;
	%geometric = 1.414;
	basicspace = 4.\pt;

	geometric = 0.;
     	arithmetic_basicspace = 2.;
        arithmetic_multiplier = 4.8\pt;
   
	%
	interbeam = 2.667\pt;

	gourlay_energybound = 100000.;
	gourlay_maxmeasures = 14.;
	castingalgorithm = \Gourlay;
	\symboltables { \table_sixteen }
	\requesttranslator { \orchestral_score_translator }
}
