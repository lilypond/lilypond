% score-bar-numbering.ly

Score = \translator {
	\type Score_engraver;
	barScriptPadding = "2.0";	% dimension \pt
	markScriptPadding = "4.0";
	barColumnPriority = "-4";
	markBreakPriority = "-4";

	\consists "Timing_engraver";
	\consists "Bar_column_engraver";
	\consists "Bar_number_engraver";
	\consists "Mark_engraver";
	\consists "Span_score_bar_engraver";
	\consists "Score_priority_engraver";
	\consists "Priority_horizontal_align_engraver";
	\consists "Vertical_align_engraver";

	\accepts "ChoireStaff";
	\accepts "StaffGroup";
	\accepts "Staff";
	\accepts "RhythmicStaff";	
	\accepts "Lyrics";
	\accepts "GrandStaff";
}

