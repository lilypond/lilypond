


\version "1.0.7";

onestaff =	\type Staff = foo\notes  {
		\property Staff.instr = instr
		\property Staff.instrument = instrument \mark "B"; c1 \mark "A"; \break c2  c2 \break }

grstaff =	\notes \type GrandStaff <
	\type Staff = bar	{

	\property Staff.instr = instr
	
	 \mark "B"; \break c1 \mark "A"; c2  }
	\type Staff = bufl	  { c1 c2  } >

scpaper =  \paper {Score = \translator {
	\type Score_engraver;
	barScriptPadding = "2.0";	% dimension \pt
	markScriptPadding = "4.0";
	barColumnPriority = "-4";
	markBreakPriority = "-4";
	barNumberBreakPriority = "-4";
	
	\consists "Timing_engraver";
	\consists "Bar_column_engraver";
	\consists "Bar_number_engraver";
	\consists "Mark_engraver";
	\consists "Span_score_bar_engraver";
	\consists "Score_priority_engraver";
	\consists "Priority_horizontal_align_engraver";
	\consists "Vertical_align_engraver";

	\accepts "ChoirStaff";
	\accepts "StaffGroup";
	\accepts "Staff";
	\accepts "RhythmicStaff";	
	\accepts "Lyrics";
	\accepts "GrandStaff";
}}

stpaper =\paper{
Staff = \translator {
	\type "Line_group_engraver_group";
	defaultclef = violin;
	barColumnPriority = "0";

	marginBreakPriority = "-4";

	\consists "Bar_engraver";
	\consists "Clef_engraver";
	\consists "Key_engraver";
	\consists "Time_signature_engraver";
	\consists "Local_key_engraver";
	\consists "Staff_sym_engraver";
	\consists "Mark_engraver";	
	\consists "Collision_engraver";
	\consists "Rest_collision_engraver";
	\consists "Bar_column_engraver";
	\consists "Bar_number_engraver";
	\consists "Separating_line_group_engraver";
	\consists "Staff_margin_engraver";
	\accepts "Voice";
}}

scscore = \score { \grstaff \paper {
\scpaper
}}


stscore = \score { \onestaff \paper {
 \stpaper
}}

%\score {\stscore}
\score {\scscore}
