\score{
	\melodic{
		c4 c c c \break
		d d d d \break
		R1*4 \break
		e4 e e e \break
		f f f f \break
	}
	\paper{
		linewidth = 40.0\mm;

% this is broken: edit init/engraver.ly
%
%{
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
%	\consists "Line_group_engraver";
	\consists "Hara_kiri_line_group_engraver";
%}
	}
	}
}
