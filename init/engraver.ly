%
% setup for Request->Element conversion. Guru-only
%

Staff = \translator {
	\type "Line_group_engraver_group";
%{
	The Hara_kiri_line_group_engraver is a Line_group_engraver 
	that will not typeset an empty line of staff, i.e., a line 
	of staff with only rests in it.  This is needed for orchestral
	scores.  Comment-out Line_group_engraver_group, and uncomment 
	Hara_kiri_line_group_engraver.
%}
%	\type "Hara_kiri_line_group_engraver";
	defaultclef = violin;

	\consists "Bar_engraver";
	\consists "Clef_engraver";
	\consists "Key_engraver";
	\consists "Local_key_engraver";
	\consists "Time_signature_engraver";
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
%{
	The Staff_margin_engraver puts the name of the instrument
	(\property Staff.instrument; Staff.instr for subsequent lines)
	to the left of a staff.
%}
%{
	marginBreakPriority = "-5";
	\consists "Staff_margin_engraver";
%}
	\consists "Separating_line_group_engraver";
	  
	\accepts "Voice";
}
ChoireStaff = \translator {
	\type "Line_group_engraver_group";
	\consists "Vertical_align_engraver";
	\consists "Staff_group_bar_engraver";
	\accepts "Staff";
	\accepts "RhythmicStaff";
	\accepts "GrandStaff";
	\accepts "Lyrics";
}


RhythmicStaff = \translator
{
	\type "Line_group_engraver_group";
	nolines  = "1";
	\consists "Pitch_squash_engraver";

	\consists "Bar_engraver";
	\consists "Time_signature_engraver";
	\consists "Staff_sym_engraver";
	\accepts "Voice";
}

Voice = \translator {
	\type "Engraver_group_engraver";
	\consists "Dynamic_engraver";
 	\consists "Rest_engraver";
	\consists "Dot_column_engraver";
	\consists "Stem_engraver";
	\consists "Plet_engraver";
	\consists "Beam_engraver";
	\consists "Abbreviation_beam_engraver";
	\consists "Multi_measure_rest_engraver";
	\consists "Script_engraver";
	\consists "Rhythmic_column_engraver";
	\consists "Font_size_engraver";
	\consists "Slur_engraver";
	\consists "Ties_engraver";
	\consists "Note_heads_engraver" ;	
	\consists "Skip_req_swallow_translator";
	%\accepts "Thread";
}


GrandStaff = \translator {
	\type "Line_group_engraver_group";

	\consists "Span_bar_engraver";
	\consists "Vertical_align_engraver";
	\consists "Piano_bar_engraver";
	minVerticalAlign = 1.5*\staffheight;

	% This should come last
	\accepts "Staff";
}

StaffGroup = \translator {
	\type "Line_group_engraver_group";
%	\type "Hara_kiri_line_group_engraver";
	\consists "Span_bar_engraver";
	\consists "Vertical_align_engraver";
	\consists "Staff_group_bar_engraver";

	\accepts "Staff";
	\accepts "RhythmicStaff";
	\accepts "GrandStaff";
	\accepts "Lyrics";
}

LyricVoice = 
\translator{
	\type "Line_group_engraver_group";

	\consists "Separating_line_group_engraver";
	\consists "Lyric_engraver";
	\consists "Beam_req_swallow_translator";
	\consists "Plet_swallow_engraver";
}

Lyrics = \translator {
	\type "Line_group_engraver_group";
	\consists "Vertical_align_engraver";
	\accepts "LyricVoice";
}

Score = \translator {
	\type Score_engraver;

	\consists "Timing_engraver";
	% uncomment to bar numbers on a whole system.
%{
	\consists "Bar_column_engraver";
	\consists "Bar_number_engraver";
%}
	\consists "Bar_column_engraver";
	\consists "Span_score_bar_engraver";
	\consists "Score_priority_engraver";
	\consists "Priority_horizontal_align_engraver";
	\consists "Vertical_align_engraver";


	\accepts "StaffGroup";
	\accepts "Staff";
	\accepts "RhythmicStaff";	
	\accepts "Lyrics";
	\accepts "GrandStaff";
	\accepts "ChoireStaff";
}

StupidScore = \translator {
	\type Score_engraver;
	\consists "Staff_sym_engraver";
}



