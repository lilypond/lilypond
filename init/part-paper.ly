%part-paper.ly

\paper {
	% for simple testing only!
%	castingalgorithm = \Wordwrap; % lots faster on my 486 -- jcn
Score = \translator {
	\type Score_engraver;

	\consists "Timing_engraver";
	\consists "Bar_column_engraver";
	\consists "Bar_number_engraver";
	\consists "Mark_engraver";
	\consists "Span_score_bar_engraver";
	\consists "Score_priority_engraver";
	\consists "Priority_horizontal_align_engraver";
	\consists "Vertical_align_engraver";

	\accepts "StaffGroup";
	\accepts "Staff";
	\accepts "RhythmicStaff";	
	\accepts "Lyrics";
	\accepts "GrandStaff";
	SkipBars = "1";
}
	\include "bar-numbering.ly";
}

