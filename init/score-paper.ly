%score-paper.ly

\paper { 
	% for simple testing only!
	castingalgorithm = \Wordwrap; % lots faster on my 486 -- jcn
Staff = \translator {
	\type "Hara_kiri_line_group_engraver";

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
	\consists "Staff_margin_engraver";
	\consists "Separating_line_group_engraver";
	  
	\accepts "Voice";
}

\include "score-bar-numbering.ly";

}
