% bar-numbering.ly
% 
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
	\consists "Collision_engraver";
	\consists "Rest_collision_engraver";
	\consists "Bar_column_engraver";
	\consists "Bar_number_engraver";
	\consists "Separating_line_group_engraver";
	  
	\accepts "Voice";
}
