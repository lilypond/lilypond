\version "1.3.122"

%
% setup for Request->Element conversion. Guru-only
%

StaffContext=\translator {
	\type "Engraver_group_engraver";
	\name Staff ;
	\consists "Output_property_engraver";	
	Generic_property_list = #generic-staff-properties
	\consists "Property_engraver";
	
	\consists "Multi_measure_rest_engraver";

	\consists "Bar_engraver";
 % Bar_engraver must be first so default bars aren't overwritten
% with empty ones.


%	\consists "Repeat_engraver";
	\consists "Volta_engraver";
	\consists "Separating_line_group_engraver";	



	\consists "Clef_engraver";
	\consists "Key_engraver";
	\consists "Time_signature_engraver";
	\consists "Staff_symbol_engraver";
	\consists "Collision_engraver";
	\consists "Rest_collision_engraver";
	\consists "Local_key_engraver";
	\consists "Piano_pedal_engraver";

%{
	The Instrument_name_engraver puts the name of the instrument
	(\property Staff.instrument; Staff.instr for subsequent lines)
	to the left of a staff.

	This is commented out, so you don't get funny things on the
	PianoStaff	
	\consists "Instrument_name_engraver";
%}

	\consistsend "Axis_group_engraver";

	  
	\accepts "Voice";
}


StaffContainerContext = \translator {
	\type Engraver_group_engraver;
	\consists "Axis_group_engraver";
	\accepts Staff;
	\name StaffContainer;
}

ChoirStaffContext = \translator {
	\type "Engraver_group_engraver";
	\name ChoirStaff;
	alignmentReference = \center;
	\consists "System_start_delimiter_engraver";
	SystemStartDelimiter \override #'glyph = #'bracket

	\accepts "Staff";
	\accepts "RhythmicStaff";
	\accepts "GrandStaff";
	\accepts "PianoStaff";
	\accepts "Lyrics";
	\accepts "ChordNames";
}


RhythmicStaffContext=\translator{
	\type "Engraver_group_engraver";
	
	\consists "Property_engraver";
	\consists "Output_property_engraver";	

	Generic_property_list = #generic-staff-properties

	\consists "Pitch_squash_engraver";
	\consists "Separating_line_group_engraver";	
	\name RhythmicStaff;
	\alias "Staff";
	
	Bar \override #'bar-size = #4
	VoltaBracket \override #'minimum-space =  #15  % urg, in \pt
	VoltaBracket \override #'padding =  #5  % urg, in \pt
	StaffSymbol \override #'line-count = #1	

%	\consists "Repeat_engraver";
	\consists "Volta_engraver";
	\consists "Bar_engraver";
	\consists "Time_signature_engraver";
	\consists "Staff_symbol_engraver";
	\consistsend "Axis_group_engraver";
	\accepts "Voice";
}


VoiceContext = \translator {
	\type "Engraver_group_engraver";
	\name Voice;

	Generic_property_list = #generic-voice-properties

	% must come before all
	\consists "Voice_devnull_engraver";
	\consists "Output_property_engraver";	
	\consists "Arpeggio_engraver";

	\consists "Text_spanner_engraver";
	\consists "Property_engraver";
	
	\consists "Breathing_sign_engraver";
 	\consists "Rest_engraver";
	\consists "Dot_column_engraver";
	\consists "Stem_engraver";
	\consists "Beam_engraver";
	\consists "Auto_beam_engraver";

	\consists "Chord_tremolo_engraver";
	\consists "Percent_repeat_engraver";
	\consists "Melisma_engraver";

%{
 Must come before text_engraver, but after note_column engraver.

%}
	\consists "Dynamic_engraver";
	\consists "Text_engraver";

	\consists "Script_engraver";
	\consists "Script_column_engraver";
	\consists "Rhythmic_column_engraver";
	\consists "Phrasing_slur_engraver";
	\consists "Slur_engraver";
	\consists "Tie_engraver";
	\consists "Tuplet_engraver";
	\consists "Grace_position_engraver";
	\consists "A2_engraver";

	\consists "Skip_req_swallow_translator";
	\accepts Thread; % bug if you leave out this!
	\accepts Grace;
}

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
	\consists "Align_note_column_engraver";

	\consists "Rhythmic_column_engraver";

	\consists "Dynamic_engraver";% in Grace ???
	\consists "Text_engraver"; % in Grace ???

	\consists "Property_engraver";

	Stem \override  #'flag-style = #"grace"
	Stem \override  #'stem-length = #6.0
	Stem \override  #'direction = #1

	NoteHead \override #'font-relative-size = #-1
	Stem \override #'font-relative-size = #-1
	Stem \override #'stem-shorten = #'(0)
	Beam \override #'font-relative-size = #-1
	TextScript \override #'font-relative-size = #-1
	Slur \override #'font-relative-size = #-1
	Accidentals \override #'font-relative-size = #-1
	Beam \override #'thickness = #0.3
	Beam \override #'space-function = #(lambda (x) 0.5)

	Stem \override #'lengths = #(map (lambda (x) (* 0.8 x)) '(3.5 3.5 3.5 4.5 5.0))
	Stem \override #'beamed-lengths =
		#'(0.0 2.5 2.0 1.5)
	Stem \override #'beamed-minimum-lengths
		 = #(map (lambda (x) (* 0.8 x)) '(0.0 2.5 2.0 1.5))

	weAreGraceContext = ##t   
	graceAccidentalSpace= 1.5 ; % in staff space
}

ThreadContext = \translator{
	\type Engraver_group_engraver;
	\name Thread;
	
	\consists "Thread_devnull_engraver";
	\consists "Note_heads_engraver";
	\consists "Note_head_line_engraver";
	\consists "Output_property_engraver";	
	Generic_property_list = #generic-thread-properties
	\consists "Property_engraver";
}

GrandStaffContext=\translator{
	\type "Engraver_group_engraver";
	\name GrandStaff;
	\consists "Span_bar_engraver";
	\consists "Span_arpeggio_engraver";
	\consists "System_start_delimiter_engraver";
	SystemStartDelimiter \override #'glyph = #'brace
	
	\consists "Property_engraver";	
	Generic_property_list = #generic-grand-staff-properties
	\accepts "Staff";
}

PianoStaffContext = \translator{
	\GrandStaffContext
	\name "PianoStaff";

	\consists "Vertical_align_engraver";

	alignmentReference = \center;
	verticalAlignmentChildCallback = #Align_interface::fixed_distance_alignment_callback
	VerticalAlignment \override #'forced-distance = #12

%	\consistsend "Axis_group_engraver";
}

StaffGroupContext= \translator {
	\type "Engraver_group_engraver";
	\name StaffGroup;

	\consists "Span_bar_engraver";
	\consists "Span_arpeggio_engraver";
	\consists "Output_property_engraver";	
	SystemStartDelimiter \override #'glyph = #'bracket

	\consists "System_start_delimiter_engraver";
	\accepts "Staff";
	\accepts "RhythmicStaff";
	\accepts "GrandStaff";
	\accepts "PianoStaff";
	
	\accepts "Lyrics";
	\accepts "ChordNames";
}


% UGH! JUNKME
LyricsVoiceContext= \translator{
	\type "Engraver_group_engraver";
	\consistsend "Axis_group_engraver";
	LyricsVoiceMinimumVerticalExtent = #(cons -1.2 1.2)

	\name LyricsVoice ;
	\consists "Separating_line_group_engraver";
	\consists "Lyric_engraver";
	\consists "Extender_engraver";
	\consists "Hyphen_engraver";
	\consists "Stanza_number_engraver";
	phrasingPunctuation = #".,;:!?\""
	
}
NoteNamesContext = \translator {
	\type "Engraver_group_engraver";
	\name NoteNames;
	\consistsend "Axis_group_engraver";
	\consists "Note_name_engraver";
	\consists "Separating_line_group_engraver";
}

LyricsContext = \translator {
	\type "Engraver_group_engraver";
	\name Lyrics;
	\consists Vertical_align_engraver; %need this for getting folded repeats right.
	Generic_property_list = #generic-lyrics-properties
	\consists "Property_engraver";
	\consistsend "Axis_group_engraver";
	
	\accepts "LyricsVoice";
}


ChordNamesContext = \translator {
	\type "Engraver_group_engraver";
	\name ChordNames;

	Generic_property_list = #generic-chord-staff-properties

	\consists "Property_engraver";	
	\consists "Output_property_engraver";	
	\consists "Separating_line_group_engraver";
	\consists "Chord_name_engraver";
	\consists "Skip_req_swallow_translator";
	\consistsend "Axis_group_engraver";

	VerticalAxisGroup \override #'invisible-staff = ##t
	}



StupidScore = \translator {
 	\type "Score_engraver";
	\name Score;
	\consists "Note_heads_engraver";
}




HaraKiriStaffContext = \translator {
	\StaffContext
	\remove "Axis_group_engraver";
	\consistsend "Hara_kiri_engraver";	  
	\consists "Instrument_name_engraver";
	\accepts "Voice";
}
%{
  The HaraKiriStaffContexts doesn't override \name,
  so it is still named `Staff'.

  %\translator { \HaraKiriStaffContext }
%}




ScoreContext = \translator {
	\type Score_engraver;
	\name Score;
	

	\consists "Repeat_acknowledge_engraver";
	\consists "Timing_engraver";
	\consists "Output_property_engraver";	
	\consists "System_start_delimiter_engraver";
	\consists "Mark_engraver";	
	\consists "Break_align_engraver";
	\consists "Spacing_engraver";
	\consists "Vertical_align_engraver";

	\consists "Lyric_phrasing_engraver";
	\consists "Bar_number_engraver";
	\consists "Span_arpeggio_engraver";

	\accepts "Staff";	
	\accepts "StaffContainer";
	\accepts "StaffGroup";
	\accepts "RhythmicStaff";	
	\accepts "Lyrics";
	\accepts "ChordNames";
	\accepts "GrandStaff";
	\accepts "ChoirStaff";
	\accepts "PianoStaff";
	\accepts "NoteNames";

	soloText = #"Solo"
	soloIIText = #"Solo II"
	aDueText = #"a2"
	soloADue = ##t
	splitInterval = #'(0 . 1)
	changeMoment = #`(,(make-moment 0 0) . ,(make-moment 1 512))

	StaffMinimumVerticalExtent = #(cons -4.0 4.0)

	barAuto = ##t
	voltaVisibility = ##t
	%  name, glyph id, clef position 
	% where is c0 in this clef?
	clefPitches = #'(("clefs-G" . -4)
	  ("clefs-C" . 0)
	  ("clefs-F" . 4)
	  ("clefs-vaticana_do" . 0)
	  ("clefs-vaticana_fa" . 4)
	  ("clefs-medicaea_do" . 0)
	  ("clefs-medicaea_fa" . 4)
	  ("clefs-hufnagel_do" . 0)
	  ("clefs-hufnagel_fa" . 4)
	  ("clefs-hufnagel_do_fa" . 0)
	  ("clefs-mensural1_c" . 0)
	  ("clefs-mensural2_c" . 0)
	  ("clefs-mensural3_c" . 0)
	  ("clefs-mensural1_f" . 4)
	  ("clefs-mensural2_f" . 4)
	  ("clefs-mensural_g" . -4))

	clefGlyph = #"clefs-G"
	clefPosition = #-2

        automaticPhrasing = ##t;
	alignmentReference = \down;
	defaultBarType = #"|"

	explicitClefVisibility = #all-visible
	explicitKeySignatureVisibility = #all-visible
	
	scriptDefinitions = #default-script-alist

	verticalAlignmentChildCallback = #Align_interface::alignment_callback

	pedalSustainStrings = #'("Ped." "*Ped." "*")
	pedalUnaChordaStrings = #'("una chorda" "" "tre chorde")
	pedalSostenutoStrings = #'()  % FIXME

	tupletNumberFormatFunction = #denominator-tuplet-formatter
	
       keyAccidentalOrder = #'(
         (6 . -1) (2  . -1) (5 . -1 ) (1  . -1) (4  . -1) (0  . -1) (3  . -1)
	 (3  . 1) (0 . 1) (4 . 1) (1 . 1) (5 . 1) (2 . 1) (6 . 1)
       )
	breakAlignOrder = #'(
	  Instrument_name
	  Left_edge_item
	  Span_bar
	  Breathing_sign
	  Clef_item
	  Key_item
	  Staff_bar
	  Time_signature
	  Custos
	  Stanza_number
	)


	\elementdescriptions #all-grob-descriptions
}

OrchestralScoreContext= \translator {
	\ScoreContext
}
EasyNotation =  \translator {
	\ScoreContext
	NoteHead \override #'molecule-callback = #Note_head::brew_ez_molecule
	easyPlay = ##t
}

