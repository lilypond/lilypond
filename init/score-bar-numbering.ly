Score = \translator {
	\type Score_engraver;

	\consists "Timing_engraver";
	\consists "Bar_column_engraver";
	\consists "Bar_number_engraver";
	\consists "Span_score_bar_engraver";
	\consists "Score_priority_engraver";
	\consists "Priority_horizontal_align_engraver";
	\consists "Vertical_align_engraver";


	\accepts "Staff_group";
	\accepts "Staff";
	\accepts "Rhythmic_staff";	
	\accepts "Lyrics";
	\accepts "Grand_staff";
}

