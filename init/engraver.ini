%
% setup for Request->Element conversion. Guru-only
%

staff_engraver = \requesttranslator {
		  Engraver "Engraver_group_engraver"
		  \alias "Staff";
		  \consists "Line_group_engraver";
		  \consists "Bar_engraver";
		  \consists "Clef_engraver";
		  \consists "Key_engraver";
		  \consists "Meter_engraver";
		  \consists "Local_key_engraver";
		  \consists "Staff_sym_engraver";
		  \consists "Collision_engraver";
		  \consists "Rest_collision_engraver";
		  \contains \requesttranslator {
			  Engraver  "Voice_group_engravers"
			  \alias "Voice_group";
			  \consists "Dynamic_engraver";
			  \consists "Stem_beam_engraver";
			  \consists "Script_engraver";
			  \consists "Note_column_engraver";
			  \consists "Slur_engraver";
			  \contains \requesttranslator {
				  Engraver "Engraver_group_engraver"
				  \alias "Voice";
				  \consists "Note_head_engraver" ;
				  \consists "Tie_engraver";
			  }
		}
	     }

piano_staff_engraver = \requesttranslator {
	Engraver "Engraver_group_engraver"
	\alias "Piano";
	\alias "Hoenoemjedat";
	\consists "Span_bar_engraver";
	\consists "Vertical_align_engraver";
	\consists "Line_group_engraver";
	\consists "Piano_bar_engraver";
	\contains\requesttranslator { \staff_engraver }
}

staff_group_engraver = \requesttranslator {
	Engraver "Engraver_group_engraver"
	\alias "Staff_group";
	\consists "Span_bar_engraver";
	\consists "Vertical_align_engraver";
	\consists "Line_group_engraver";
	\contains\requesttranslator { \staff_engraver }
}
lyric_engraver = \requesttranslator {
	Engraver "Engraver_group_engraver"
	\alias "Lyric";

	\contains\requesttranslator{
		Engraver "Engraver_group_engraver"
		\consists "Lyric_engraver";
		\consists "Line_group_engraver";
		\consists "Swallow_engraver";
	}
	\consists "Vertical_align_engraver";
}

orchestral_score_translator = \requesttranslator {
	Engraver Score_engraver
	\alias "Score";

	\consists "Bar_align_engraver";
	\consists "Clef_align_engraver";
	\consists "Key_align_engraver";
	\consists "Meter_align_engraver";
	\consists "Score_horizontal_align_engraver";
	\consists "Vertical_align_engraver";
	\consists "Span_score_bar_engraver";

	\contains \requesttranslator { \staff_group_engraver }
	\contains \requesttranslator { \lyric_engraver }
	\contains \requesttranslator { \piano_staff_engraver }

	
}



