%
% setup for Request->Element conversion. Guru-only
%

StaffContext=\translator {
	\type "Engraver_group_engraver";
	\name Staff ;
	
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
	  ("treble" "treble" -2)
	  ("violin" "treble" -2)
	  ("G" "treble" -2)
	  ("G2" "treble" -2)
	  ("french" "treble" -4 )
	  ("soprano" "alto" -4 )
	  ("mezzosoprano" "alto" -2 )
	  ("alto" "alto" 0 )
	  ("tenor" "alto" 2 )
	  ("baritone" "alto" 4 )
	  ("varbaritone" "bass" 0)
	  ("bass" "bass" 2 )
	  ("F" "bass" 2)
	  ("subbass" "bass" 4)
	)
	
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

	marginBreakPriority = #-5

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
	
	Generic_property_list = #generic-staff-properties
	
	barSize = \staffheight;
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
	weAreGraceContext = ##t 
	fontSize = #-1
	
	stemLength = #6.0
	verticalDirection = \up ;
	graceAccidentalSpace= 1.5 * \interline;
};

\translator{\GraceContext}
\translator {\VoiceContext}

ThreadContext = \translator{
	\type Engraver_group_engraver;
	\consists "Note_heads_engraver" ;
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
	barColumnPriority = #0
	marginBreakPriority = #-4
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
	barColumnPriority = #0
	marginBreakPriority = #-4
	\consists "Mark_engraver";
	\consists "Bar_number_engraver";
};

ScoreContext = \translator {
	\type Score_engraver;
	\name Score;

	\consists "Timing_engraver";

	\consists "Span_score_bar_engraver";
	\consists "Score_priority_engraver";
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

	clefBreakPriority = #-2
	breathingSignBreakPriority = #-4
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
