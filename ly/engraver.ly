%
% setup for Request->Element conversion. Guru-only
%

StaffContext=\translator {
	\type "Engraver_group_engraver";
	\name Staff ;
	\consists "Output_property_engraver";	
	barAuto = ##t
	voltaVisibility = ##t
	Generic_property_list = #generic-staff-properties
	\consists "Property_engraver";
	
	\consists "Multi_measure_rest_engraver";
	\consists "Bar_engraver";
 % Bar_engraver must be first so default bars aren't overwritten
% with empty ones.
	\consists "Repeat_engraver";


	%  name, glyph id, c0 position
	supportedClefTypes = #'(
	  ("treble" . ("G" -2))
	  ("violin" . ("G" -2))
	  ("G" . ("G" -2))
	  ("G2" . ("G" -2))
	  ("french" . ("G" -4 ))
	  ("soprano" . ("C" -4 ))
	  ("mezzosoprano" . ("C" -2 ))
	  ("alto" . ("C" 0 ))
	  ("tenor" . ("C" 2 ))
	  ("baritone" . ("C" 4 ))
	  ("varbaritone"  . ("F" 0))
	  ("bass" . ("F" 2 ))
	  ("F" . ( "F" 2))
	  ("subbass" . ("F" 4))
	)
	clefPitches = #'(("G" . -4)
	  ("C" . 0)
	  ("F" . 4))
	  
	\consists "Clef_engraver";
	\consists "Key_engraver";
	\consists "Time_signature_engraver";
	\consists "Staff_symbol_engraver";
	\consists "Collision_engraver";
	\consists "Rest_collision_engraver";
	\consists "Local_key_engraver";

	\consistsend "Axis_group_engraver";



%{
	The Staff_margin_engraver puts the name of the instrument
	(\property Staff.instrument; Staff.instr for subsequent lines)
	to the left of a staff.
%}
%{
	\consists "Staff_margin_engraver";
%}
	defaultClef = #"treble"

	\consists "Separating_line_group_engraver";
	  
	\accepts "Voice";
	dynamicStyle = #"dynamic"
};

\translator{\StaffContext }
\translator {
	\type "Engraver_group_engraver";
	\name ChoirStaff;
	alignmentReference = \center;	
	\consists "Staff_group_bar_engraver";


	\accepts "Staff";
	\accepts "RhythmicStaff";
	\accepts "GrandStaff";
	\accepts "PianoStaff";

		
	\accepts "Lyrics";
	\accepts "ChordNames";
}


RhythmicStaffContext=\translator{
	\type "Engraver_group_engraver";
	numberOfStaffLines  = #1
	\consists "Property_engraver";
	\consists "Output_property_engraver";	

	Generic_property_list = #generic-staff-properties
	
	barSize = #'staff-height
	\consists "Pitch_squash_engraver";
	\consists "Separating_line_group_engraver";	
	\name RhythmicStaff;

	\consists "Repeat_engraver";
	\consists "Bar_engraver";
	\consists "Time_signature_engraver";
	\consists "Staff_symbol_engraver";
	\consistsend "Axis_group_engraver";
	\accepts "Voice";
};
\translator{\RhythmicStaffContext}
VoiceContext = \translator {

	\type "Engraver_group_engraver";
	dynamicPadding = #5.0
	Generic_property_list = #generic-voice-properties
	\consists "Output_property_engraver";	

	\consists "Dynamic_engraver";   % must come before text_engraver.
	\name Voice ;
	\consists "Property_engraver";
	
	\consists "Breathing_sign_engraver";
 	\consists "Rest_engraver";
	\consists "Dot_column_engraver";
	\consists "Stem_engraver";
	\consists "Beam_engraver";
	\consists "Auto_beam_engraver";
	\include "auto-beam-settings.ly";
	\consists "Chord_tremolo_engraver";

	\consists "Melisma_engraver";
	textScriptPadding = #3.0
	\consists "Text_engraver";
	\consists "Script_engraver";
	\consists "Script_column_engraver";
	\consists "Rhythmic_column_engraver";
	\consists "Slur_engraver";
	\consists "Tie_engraver";
	\consists "Tuplet_engraver";
	\consists "Grace_position_engraver";
	\consists "Skip_req_swallow_translator";
	\accepts Thread; % bug if you leave out this!
	\accepts Grace;
};

GraceContext=\translator {
	\type "Grace_engraver_group";
	\name "Grace";
	\consists "Output_property_engraver";	

	Generic_property_list = #generic-grace-properties
	
	\consists "Note_heads_engraver";
	\consists "Local_key_engraver";
	\consists "Stem_engraver";
	\consists "Beam_engraver";
	\consists "Slur_engraver";
	
	\consists "Auto_beam_engraver";
	\include "auto-beam-settings.ly";
	\consists "Align_note_column_engraver";

	\consists "Rhythmic_column_engraver";
	\consists "Dynamic_engraver";

	\consists "Property_engraver";

	stemStyle = #"grace"
	flagStyle = #"grace" 
	weAreGraceContext = ##t 
	fontSize = #-1
	
	stemLength = #6.0
	verticalDirection = \up ;
	graceAccidentalSpace= 1.5 * \staffspace;
};

\translator{\GraceContext}
\translator {\VoiceContext}

ThreadContext = \translator{
	\type Engraver_group_engraver;
	\consists "Note_heads_engraver" ;
	\consists "Output_property_engraver";	
	Generic_property_list = #generic-thread-properties
	\consists "Property_engraver";
	\name Thread;
};

\translator{\ThreadContext}
GrandStaffContext=\translator{
	\type "Engraver_group_engraver";
	\name GrandStaff;
	\consists "Span_bar_engraver";
	\consists "Piano_bar_engraver";
	\consists "Property_engraver";	
	Generic_property_list = #generic-grand-staff-properties
	\accepts "Staff";
};
\translator{\GrandStaffContext}

PianoStaffContext = \translator{\GrandStaffContext
	alignmentReference = \center;

	\consists "Vertical_align_engraver";
	minVerticalAlign = 3.0*\staffheight;
	maxVerticalAlign = 3.0*\staffheight;

%	\consistsend "Axis_group_engraver";
	\name "PianoStaff";
	
};
\translator{\PianoStaffContext}
StaffGroupContext= \translator {
	\type "Engraver_group_engraver";
	\consists "Span_bar_engraver";
	\consists "Output_property_engraver";	
	
	\consists "Staff_group_bar_engraver";
	\name StaffGroup;
	\accepts "Staff";
	\accepts "RhythmicStaff";
	\accepts "GrandStaff";
	\accepts "PianoStaff";
	
	\accepts "Lyrics";
	\accepts "ChordNames";
};

\translator { \StaffGroupContext }

% UGH! JUNKME
LyricsVoiceContext= \translator{
	\type "Engraver_group_engraver";
	\consistsend "Axis_group_engraver";

	\name LyricVoice ;
	\consists "Separating_line_group_engraver";
	\consists "Lyric_engraver";
	\consists "Extender_engraver";
	\consists "Hyphen_engraver";
};
\translator{ \LyricsVoiceContext }

LyricsContext = \translator {
	\type "Engraver_group_engraver";
	\name Lyrics;
	\consists Vertical_align_engraver;%need this for getting folded repeats right.

	\consistsend "Axis_group_engraver";
	
	\accepts "LyricVoice";
};
\translator { \LyricsContext }

\translator{
	\type "Engraver_group_engraver";
	\consists "Output_property_engraver";	

	\consistsend "Axis_group_engraver";
	\name ChordNameVoice ;
	\consists "Separating_line_group_engraver";
	\consists "Chord_name_engraver";
}


ChordNameContext = \translator {
	\type "Engraver_group_engraver";
	\name ChordNames;
	\accepts "ChordNameVoice";
	\consistsend "Axis_group_engraver";
	};
\translator { \ChordNameContext }


ScoreWithNumbers = \translator {
 	\type "Score_engraver";

	% uncomment to bar numbers on a whole system.
	\consists "Bar_number_engraver";
};

StupidScore = \translator {
 	\type "Score_engraver";
	\name Score;
	\consists "Note_heads_engraver";
};



BarNumberingStaffContext = \translator {
	\StaffContext
	\consists "Mark_engraver";
	\consists "Bar_number_engraver";
};

HaraKiriStaffContext = \translator {
	\StaffContext
	\remove "Axis_group_engraver";
	\consistsend "Hara_kiri_engraver";	  
	\accepts "Voice";
};

OrchestralPartStaffContext = \translator {
	\StaffContext
	\consists "Mark_engraver";
	\consists "Bar_number_engraver";
};

ScoreContext = \translator {
	\type Score_engraver;
	\name Score;

	\consists "Timing_engraver";
	\consists "Output_property_engraver";	

	\consists "Span_score_bar_engraver";
%	\consists "Score_priority_engraver";
	\consists "Break_align_engraver";
	breakAlignOrder = #'(
	  "Span_bar"
	  "Breathing_sign"
	  "Clef_item"
	  "Key_item"
	  "Staff_bar"
	  "Time_signature"
	)
	\consists "Spacing_engraver";
	\consists "Vertical_align_engraver";
	alignmentReference = \down;
	defaultClef = #"treble"
	defaultBarType = #"|"
	\accepts "Staff";
	\accepts "StaffGroup";
	\accepts "RhythmicStaff";	
	\accepts "Lyrics";
	\accepts "ChordNames";
	\accepts "GrandStaff";
	\accepts "ChoirStaff";
	\accepts "PianoStaff";
	\accepts "NoteNames";

	markVisibilityFunction = #end-of-line-invisible
	barNumberVisibilityFunction = #end-of-line-invisible
	marginVisibilityFunction = #begin-of-line-visible
};

\translator { \ScoreContext }

OrchestralScoreContext= \translator {
	\ScoreContext

	barScriptPadding = #2.0		% dimension \pt
	markScriptPadding = #4.0

	\consists "Bar_number_engraver";
	\consists "Mark_engraver";

	\accepts "HaraKiriStaff";
};

\translator {
	\type "Engraver_group_engraver";
	\name NoteNames;
	\consistsend "Axis_group_engraver";
	\consists "Note_name_engraver";
}
