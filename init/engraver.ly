%
% setup for Request->Element conversion. Guru-only
%

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
%{
	Uncomment to get bar numbers on single staff systems:
	
	The Bar_number_engraver puts a number over a staff created
	at the same level of hierarchy.  This why you have to add it
	here separately if you want to have numbers on single staff
	systems: The Bar_number_engraver in Score_engraver will only
	put numbers on bars that are Score ("system") wide.  Such
	bars are only created when the toplevel system has multiple
	children-staffs.

%}
%{
	\consists "Bar_column_engraver";
	\consists "Bar_number_engraver";
%}
	\consists "Separating_line_group_engraver";
	\consists "Line_group_engraver";
	  
	\accepts "Voice";
}

Rhythmic_staff = \translator
{
	  \type "Engraver_group_engraver";
	nolines  = "1";
	  \consists "Pitch_squash_engraver";

	  \consists "Bar_engraver";
	  \consists "Meter_engraver";
	  \consists "Staff_sym_engraver";
	  \consists "Line_group_engraver";
	  \accepts "Voice";
}
Voice = \translator {
	\type "Engraver_group_engraver";
	\consists "Dynamic_engraver";
 	\consists "Rest_engraver";
	\consists "Stem_engraver";
	\consists "Plet_engraver";
	\consists "Beam_engraver";
	\consists "Abbreviation_beam_engraver";
	\consists "Script_engraver";
	\consists "Rhythmic_column_engraver";
	\consists "Slur_engraver";
	\accepts "Thread";
}

Thread = \translator {
	\type "Engraver_group_engraver";
	\consists "Skip_req_swallow_translator";
	\consists "Note_head_engraver" ;
	\consists "Tie_engraver";
}

Grandstaff = \translator {
	\type "Engraver_group_engraver";

	\consists "Span_bar_engraver";
	\consists "Vertical_align_engraver";
	\consists "Piano_bar_engraver";

	% This should come last
	\consists "Line_group_engraver";
	\accepts "Staff";
}

Staff_group = \translator {
	\type "Engraver_group_engraver";
	\consists "Span_bar_engraver";
	\consists "Vertical_align_engraver";
	\consists "Staff_group_bar_engraver";
	\consists "Line_group_engraver";
	\accepts "Staff";
	\accepts "Rhythmic_staff";
	\accepts "Grandstaff";
	\accepts "Lyrics";
}

Lyric_voice = 
\translator{
	\type "Engraver_group_engraver";

	\consists "Separating_line_group_engraver";
	\consists "Lyric_engraver";
	\consists "Line_group_engraver";
	\consists "Beam_req_swallow_translator";
	\consists "Plet_swallow_engraver";
}

Lyrics = \translator {
	\type "Engraver_group_engraver";
	\consists "Vertical_align_engraver";
	\consists "Line_group_engraver";
	\accepts "Lyric_voice";
}

Score = \translator {
	\type Score_engraver;

	\consists "Timing_engraver";
	% uncomment to bar numbers on a whole system.
%{
	\consists "Bar_column_engraver";
	\consists "Bar_number_engraver";
%}
	\consists "Span_score_bar_engraver";
	\consists "Score_priority_engraver";
	\consists "Priority_horizontal_align_engraver";
	\consists "Vertical_align_engraver";


	\accepts "Staff_group";
	\accepts "Staff";
	\accepts "Rhythmic_staff";	
	\accepts "Lyrics";
	\accepts "Grandstaff";
}

Stupid_score = \translator {
	\type Score_engraver;
	\consists "Staff_sym_engraver";
}



