\score{
	\melodic { R1 }
	\paper{
		linewidth = 40.0\mm;

Staff = \translator {
	\type "Hara_kiri_line_group_engraver";
	defaultclef = violin;

	\consists "Bar_engraver";
	\consists "Clef_engraver";
	\consists "Key_engraver";
	\consists "Meter_engraver";
	\consists "Local_key_engraver";
	\consists "Staff_sym_engraver";
	\consists "Collision_engraver";
	\consists "Rest_collision_engraver";
% {
	\consists "Bar_column_engraver";
	\consists "Bar_number_engraver";
% }
	\consists "Staff_margin_engraver";
	\consists "Separating_line_group_engraver";
%	\consists "Line_group_engraver";
	  
	\accepts "Voice";
}

% }
	}
}

