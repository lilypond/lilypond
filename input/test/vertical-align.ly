
\version "1.0.3";

\score {
\notes <
	\type GrandStaff < {
	   \property GrandStaff . maxVerticalAlign = "60."
	   \property GrandStaff . minVerticalAlign = "35."

	   c'1 \break  c'''''1 
	}
	    { c'1 \break c,,,,1}
	>

>

\paper{
Staff = \translator {
	\type "Line_group_engraver_group";
	defaultclef = violin;

	\consists "Bar_engraver";

	\consists "Key_engraver";
	\consists "Local_key_engraver";
	\consists "Time_signature_engraver";
	\consists "Staff_sym_engraver";
	\consists "Collision_engraver";
	\consists "Rest_collision_engraver";
	\consists "Separating_line_group_engraver";
	  
	\accepts "Voice";
}
}
}
