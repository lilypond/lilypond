part = \melodic{
	\octave c;
	c c c c
	r1*3
	c4 c c c
	c c c c
	c c c c
	c c c c
}

a4 = \paper{
	linewidth= 80.\mm;
	Staff = \translator {
		\type "Engraver_group_engraver";
		defaultclef = violin;
		\consists "Bar_engraver";
		\consists "Clef_engraver";
		\consists "Key_engraver";
		\consists "Meter_engraver";
		\consists "Local_key_engraver";
		\consists "Staff_sym_engraver";
		\consists "Collision_engraver";
		\consists "Rest_collision_engraver";
		\consists "Bar_column_engraver";
		\consists "Bar_number_engraver";
		\consists "Separating_line_group_engraver";
		\consists "Line_group_engraver";
		\accepts "Voice";
	}
}

\score{
	<
		\melodic{ 
			\property Score.part = 1
			\part
		}
	>
	\paper{\a4}
}

\score{
	<
		\melodic{ 
			\property Score.part = 0
			\part
		}
	>
	\paper{\a4}
}
