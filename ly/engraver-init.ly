\version "1.3.146"

%
% setup for Request->Element conversion. Guru-only
%

StaffContext=\translator {
	\type "Engraver_group_engraver"
	\name Staff 
	\consists "Output_property_engraver"	
	Generic_property_list = #generic-staff-properties
	
	\consists "Bar_engraver"
 % Bar_engraver must be first so default bars aren't overwritten
% with empty ones.
	\consists "Font_size_engraver"

%	\consists "Repeat_engraver"
	\consists "Volta_engraver"
	\consists "Separating_line_group_engraver"	
	SeparatingGroupSpanner \override #'spacing-procedure
	  =  #Separating_group_spanner::set_spacing_rods_and_seqs


	\consists "Clef_engraver"
	\consists "Key_engraver"
	\consists "Time_signature_engraver"
	\consists "Staff_symbol_engraver"
	\consists "Collision_engraver"
	\consists "Rest_collision_engraver"
	\consists "Accidental_engraver"
	\consists "Piano_pedal_engraver"
	\consists "Instrument_name_engraver"

	\consistsend "Axis_group_engraver"
	
	MinimumVerticalExtent = #'(-4 . 4)
	ExtraVerticalExtent = ##f
	VerticalExtent = ##f 

	% explicitly set instrument, so we don't get 
	% weird effects when doing instrument names for
	% piano staves

	instrument = ##f
	instr = ##f
	  
	\accepts "Voice"
}


StaffContainerContext = \translator {
	\type Engraver_group_engraver
	\consists "Axis_group_engraver"
	MinimumVerticalExtent = ##f
	ExtraVerticalExtent = ##f
	VerticalExtent = ##f 
	
	\accepts Staff
	\name StaffContainer
}

InnerChoirStaffContext = \translator {
	\type "Engraver_group_engraver"
	\name InnerChoirStaff
	alignmentReference = #0
	\consists "System_start_delimiter_engraver"
	SystemStartDelimiter = #'SystemStartBracket

	\accepts "Staff"
	\accepts "RhythmicStaff"
	\accepts "GrandStaff"
	\accepts "PianoStaff"
	\accepts "Lyrics"
	\accepts "ChordNames"
}
ChoirStaffContext = \translator {
	\InnerChoirStaffContext
	\name ChoirStaff
	\accepts "InnerChoirStaff"
	\accepts "InnerStaffGroup"
}


RhythmicStaffContext=\translator{
	\type "Engraver_group_engraver"
	
	\consists "Output_property_engraver"	

	Generic_property_list = #generic-staff-properties
	MinimumVerticalExtent = ##f
	ExtraVerticalExtent = ##f
	VerticalExtent = ##f 

	\consists "Pitch_squash_engraver"
	\consists "Separating_line_group_engraver"	
	\name RhythmicStaff
	\alias "Staff"
	
	Bar \override #'bar-size = #4
	VoltaBracket \override #'minimum-space =  #15  % urg, in \pt
	VoltaBracket \override #'padding =  #5  % urg, in \pt
	StaffSymbol \override #'line-count = #1	

%	\consists "Repeat_engraver"
	\consists "Volta_engraver"
	\consists "Bar_engraver"
	\consists "Time_signature_engraver"
	\consists "Staff_symbol_engraver"
	\consistsend "Axis_group_engraver"
	\accepts "Voice"
}


VoiceContext = \translator {
	\type "Engraver_group_engraver"
	\name Voice

	Generic_property_list = #generic-voice-properties
	\consists "Font_size_engraver"
	
	% must come before all
	\consists "Voice_devnull_engraver"
	\consists "Output_property_engraver"	
	\consists "Arpeggio_engraver"
	\consists "Multi_measure_rest_engraver"
	\consists "Text_spanner_engraver"
	
	\consists "Breathing_sign_engraver"
 	% \consists "Rest_engraver"
	\consists "Dot_column_engraver"
	\consists "Stem_engraver"
	\consists "Beam_engraver"
	\consists "Auto_beam_engraver"

	\consists "Chord_tremolo_engraver"
	\consists "Percent_repeat_engraver"
	\consists "Melisma_engraver"

%{
 Must come before text_engraver, but after note_column engraver.

%}
	\consists "Dynamic_engraver"
	\consists "Text_engraver"

	\consists "Script_engraver"
	\consists "Script_column_engraver"
	\consists "Rhythmic_column_engraver"
	\consists "Phrasing_slur_engraver"
	\consists "Slur_engraver"
	\consists "Tie_engraver"
	\consists "Porrectus_engraver"
	\consists "Tuplet_engraver"
	\consists "A2_engraver"

	\consists "Skip_req_swallow_translator"
	\accepts Thread % bug if you leave out this!
}

ThreadContext = \translator{
	\type Engraver_group_engraver
	\name Thread

	\consists "Font_size_engraver"	
	\consists "Thread_devnull_engraver"
	\consists "Note_heads_engraver"
	\consists "Rest_engraver"
	\consists "Note_head_line_engraver"
	\consists "Output_property_engraver"	
	Generic_property_list = #generic-thread-properties
}

GrandStaffContext=\translator{
	\type "Engraver_group_engraver"
	\name GrandStaff
	\consists "Span_bar_engraver"
	\consists "Span_arpeggio_engraver"
	\consists "System_start_delimiter_engraver"
	SystemStartDelimiter = #'SystemStartBrace
	Generic_property_list = #generic-grand-staff-properties
	\accepts "Staff"
}

PianoStaffContext = \translator{
	\GrandStaffContext
	\name "PianoStaff"

	\consists "Vertical_align_engraver"
	\consists "Instrument_name_engraver"
	
	instrument = ##f
	instr = ##f
	
	verticalAlignmentChildCallback = #Align_interface::fixed_distance_alignment_callback
	VerticalAlignment \override #'forced-distance = #12
	VerticalAlignment \override #'self-alignment-Y = #0
%	\consistsend "Axis_group_engraver"
}

InnerStaffGroupContext= \translator {
	\type "Engraver_group_engraver"
	\name InnerStaffGroup

	\consists "Span_bar_engraver"
	\consists "Span_arpeggio_engraver"
	\consists "Output_property_engraver"	
	SystemStartDelimiter = #'SystemStartBracket

	\consists "System_start_delimiter_engraver"
	\accepts "Staff"
	\accepts "RhythmicStaff"
	\accepts "GrandStaff"
	\accepts "PianoStaff"
	
	\accepts "Lyrics"
	\accepts "ChordNames"
}
StaffGroupContext = \translator {
	\InnerStaffGroupContext
	\name StaffGroup
	\accepts "InnerChoirStaff"
	\accepts "ChoirStaff"
	\accepts "InnerStaffGroup"
	\accepts "FiguredBass"
}


% UGH! JUNKME
LyricsVoiceContext= \translator{
	\type "Engraver_group_engraver"
	\consistsend "Axis_group_engraver"
	MinimumVerticalExtent = #(cons -1.2 1.2)
	ExtraVerticalExtent = ##f
	VerticalExtent = ##f 
	\name LyricsVoice 
	\consists "Separating_line_group_engraver"
	\consists "Lyric_engraver"
	\consists "Extender_engraver"
	\consists "Hyphen_engraver"
	\consists "Stanza_number_engraver"
	phrasingPunctuation = #".,:!?\""
	
}
NoteNamesContext = \translator {
	\type "Engraver_group_engraver"
	\name NoteNames
	\consistsend "Axis_group_engraver"

	MinimumVerticalExtent = ##f
	ExtraVerticalExtent = ##f
	VerticalExtent = ##f 

	
	\consists "Note_name_engraver"
	\consists "Separating_line_group_engraver"
}

LyricsContext = \translator {
	\type "Engraver_group_engraver"
	\name Lyrics
	\consists Vertical_align_engraver %need this for getting folded repeats right.
	Generic_property_list = #generic-lyrics-properties

	\consistsend "Axis_group_engraver"
	MinimumVerticalExtent = ##f
	ExtraVerticalExtent = ##f
	VerticalExtent = ##f 
	
	\accepts "LyricsVoice"
}


ChordNamesContext = \translator {
	\type "Engraver_group_engraver"
	\name ChordNames

	Generic_property_list = #generic-chord-staff-properties


	\consists "Output_property_engraver"	
	\consists "Separating_line_group_engraver"
	\consists "Chord_name_engraver"
	\consists "Skip_req_swallow_translator"
	\consistsend "Axis_group_engraver"
	MinimumVerticalExtent = ##f
	ExtraVerticalExtent = ##f
	VerticalExtent = ##f 

	VerticalAxisGroup \override #'invisible-staff = ##t
	}



StupidScore = \translator {
 	\type "Score_engraver"
	\name Score
	\consists "Note_heads_engraver"
}




HaraKiriStaffContext = \translator {
	\StaffContext
	\remove "Axis_group_engraver"
	\consistsend "Hara_kiri_engraver"
	\consists "Instrument_name_engraver"
	\accepts "Voice"
}
%{
  The HaraKiriStaffContexts doesn't override \name,
  so it is still named `Staff'.

  %\translator { \HaraKiriStaffContext }
%}




ScoreContext = \translator {
	\type Score_engraver
	\name Score
	
	\consists "Repeat_acknowledge_engraver"
	\consists "Timing_engraver"
	\consists "Output_property_engraver"
	\consists "System_start_delimiter_engraver"
	\consists "Mark_engraver"	
	\consists "Break_align_engraver"
	\consists "Spacing_engraver"
	\consists "Vertical_align_engraver"
	\consists "Lyric_phrasing_engraver"
	\consists "Bar_number_engraver"
	\consists "Span_arpeggio_engraver"

	\accepts "Staff"
	\accepts "StaffContainer"
	\accepts "StaffGroup"
	\accepts "RhythmicStaff"	
	\accepts "Lyrics"
	\accepts "ChordNames"
	\accepts "GrandStaff"
	\accepts "ChoirStaff"
	\accepts "PianoStaff"
	\accepts "NoteNames"
	\accepts "FiguredBass"	

	soloText = #"Solo"
	soloIIText = #"Solo II"
	aDueText = #"a2"
	soloADue = ##t
	splitInterval = #'(0 . 1)
	changeMoment = #`(,(make-moment 0 0) . ,(make-moment 1 512))
	SystemStartDelimiter =#'SystemStartBar
	barAuto = ##t
	voltaVisibility = ##t
	%  name, glyph id, clef position 
	% where is c0 in this clef?

	clefGlyph = #"clefs-G"
	clefPosition = #-2
	centralCPosition = #-6
	
        automaticPhrasing = ##t
	alignmentReference = #-1   % \down
	defaultBarType = #"|"

	explicitClefVisibility = #all-visible
	explicitKeySignatureVisibility = #all-visible
	autoBeamSettings = #auto-beam-settings

	scriptDefinitions = #default-script-alist

	verticalAlignmentChildCallback = #Align_interface::alignment_callback

	pedalSustainStrings = #'("Ped." "*Ped." "*")
	pedalUnaCordaStrings = #'("una corda" "" "tre corde")
	pedalSostenutoStrings = #'()  % FIXME

	tupletNumberFormatFunction = #denominator-tuplet-formatter
	
	subdivideBeams = ##f

       keyAccidentalOrder = #'(
         (6 . -1) (2  . -1) (5 . -1 ) (1  . -1) (4  . -1) (0  . -1) (3  . -1)
	 (3  . 1) (0 . 1) (4 . 1) (1 . 1) (5 . 1) (2 . 1) (6 . 1)
         (6 . -2) (2  . -2) (5 . -2 ) (1  . -2) (4  . -2) (0  . -2) (3 . -2)
         (3  . 2) (0 . 2) (4 . 2) (2 . 2) (5 . 2) (2 . 2) (6 . 2)
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
	)


	\grobdescriptions #all-grob-descriptions
}

OrchestralScoreContext= \translator {
	\ScoreContext
}
EasyNotation =  \translator {
	\ScoreContext
	NoteHead \override #'molecule-callback = #Note_head::brew_ez_molecule
	easyPlay = ##t
}

% retain for compatibility reasons (FIXME: convert-ly)
GraceContext = \translator {
	\type "Engraver_group_engraver"
}

FiguredBassContext = \translator {
	\type "Engraver_group_engraver"
	\name FiguredBass 
	\consists "Figured_bass_engraver"	
	\consistsend "Axis_group_engraver"
}
